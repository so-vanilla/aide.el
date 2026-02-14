;;; aide-session-status.el --- Claude Code session status tracking -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Omura Shuto

;; Author: Omura Shuto <somura-vanilla@so-icecream.com>
;; Maintainer: Omura Shuto <somura-vanilla@so-icecream.com>
;; URL: https://github.com/so-vanilla/aide
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (perspective "2.0"))
;; Keywords: tools, convenience

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Claude Code の hook 経由で書き出されるセッションステータスファイルを
;; 監視し、各 perspective の Claude Code セッション状態（working/waiting/idle）
;; を提供するモジュール。
;;
;; file-notify (inotify) でディレクトリを監視し、フォールバックでポーリング。
;; aide-persp-sidebar と連携してサイドバーにステータスを表示する。

;;; Code:

(require 'json)
(require 'filenotify)
(require 'perspective)

(declare-function claude-code-ide--get-buffer-name "claude-code-ide")
(defvar claude-code-ide--processes)

;;;; Customization

(defgroup aide-session-status nil
  "Claude Code session status tracking."
  :group 'claude-code-ide
  :prefix "aide-session-status-")

(defcustom aide-session-status-data-directory "/tmp/claude-code-status/"
  "Directory where session-status.sh writes status JSON files."
  :type 'directory)

(defcustom aide-session-status-poll-interval 5.0
  "Fallback polling interval in seconds when file-notify is unavailable."
  :type 'number)

;;;; Faces

(defface aide-session-status-working
  '((t :foreground "#61afef"))
  "Face for working state (Claude is processing).")

(defface aide-session-status-waiting
  '((t :foreground "#e5c07b"))
  "Face for waiting state (awaiting user response).")

(defface aide-session-status-idle
  '((t :foreground "#98c379"))
  "Face for idle state (ready for input).")

(defface aide-session-status-none
  '((t :foreground "#5c6370"))
  "Face for no session state.")

;;;; Internal variables

(defvar aide-session-status--cache (make-hash-table :test 'equal)
  "Cache of session status data, keyed by project-dir.")

(defvar aide-session-status--file-watch nil
  "File notify descriptor for the status directory.")

(defvar aide-session-status--poll-timer nil
  "Fallback polling timer.")

(defvar aide-session-status--change-hook nil
  "Hook run when any session status changes.
Called with no arguments.")

;;;; Data reading

(defun aide-session-status--data-file (project-dir)
  "Return the status file path for PROJECT-DIR."
  (expand-file-name
   (concat (md5 (directory-file-name project-dir)) ".json")
   aide-session-status-data-directory))

(defun aide-session-status--read-file (file)
  "Read and parse status JSON FILE.
Returns parsed data as alist, or nil on failure."
  (when (file-readable-p file)
    (condition-case nil
        (let ((json-object-type 'alist)
              (json-key-type 'symbol))
          (json-read-file file))
      (error nil))))

(defun aide-session-status--update-from-file (file)
  "Update cache from status FILE.  Return non-nil if changed."
  (let* ((data (aide-session-status--read-file file))
         (project-dir (when data (alist-get 'project_dir data)))
         (new-timestamp (when data (alist-get 'timestamp data)))
         (old-data (when project-dir
                     (gethash project-dir aide-session-status--cache)))
         (old-timestamp (when old-data (alist-get 'timestamp old-data))))
    (when (and project-dir data
              (or (null old-timestamp)
                  (null new-timestamp)
                  (>= new-timestamp old-timestamp)))
      (puthash project-dir data aide-session-status--cache)
      t)))

(defun aide-session-status--handle-deletion (file)
  "Handle deletion of status FILE by removing from cache."
  (let ((basename (file-name-sans-extension (file-name-nondirectory file)))
        (dirs-to-remove nil))
    (maphash
     (lambda (dir _data)
       (when (string= (md5 (directory-file-name dir)) basename)
         (push dir dirs-to-remove)))
     aide-session-status--cache)
    (dolist (dir dirs-to-remove)
      (remhash dir aide-session-status--cache))
    (consp dirs-to-remove)))

(defun aide-session-status--scan-directory ()
  "Scan the status directory and update all cached data."
  (let ((dir aide-session-status-data-directory)
        (changed nil))
    (when (file-directory-p dir)
      ;; Update from existing files
      (dolist (file (directory-files dir t "\\.json\\'"))
        (when (aide-session-status--update-from-file file)
          (setq changed t)))
      ;; Remove stale entries whose files no longer exist
      (let ((stale-dirs nil))
        (maphash
         (lambda (project-dir _data)
           (unless (file-exists-p (aide-session-status--data-file project-dir))
             (push project-dir stale-dirs)))
         aide-session-status--cache)
        (dolist (dir stale-dirs)
          (remhash dir aide-session-status--cache)
          (setq changed t))))
    (when changed
      (run-hooks 'aide-session-status--change-hook))))

;;;; File notify

(defun aide-session-status--on-notify (event)
  "Handle file-notify EVENT for the status directory."
  (let ((action (nth 1 event))
        (file (nth 2 event)))
    (when (eq action 'renamed)
      (setq file (nth 3 event)))
    (when (and file (string-suffix-p ".json" file))
      (let ((changed
             (pcase action
               ((or 'created 'changed 'renamed)
                (aide-session-status--update-from-file file))
               ('deleted
                (aide-session-status--handle-deletion file))
               (_ nil))))
        (when changed
          (run-hooks 'aide-session-status--change-hook))))))

(defun aide-session-status--start-watching ()
  "Start watching the status directory."
  (let ((dir aide-session-status-data-directory))
    (make-directory dir t)
    (condition-case nil
        (setq aide-session-status--file-watch
              (file-notify-add-watch
               dir '(change)
               #'aide-session-status--on-notify))
      (error nil))
    ;; inotify 成否に関わらずポーリングも起動
    (setq aide-session-status--poll-timer
          (run-with-timer 0 aide-session-status-poll-interval
                          #'aide-session-status--scan-directory))
    (aide-session-status--scan-directory)))

(defun aide-session-status--stop-watching ()
  "Stop watching the status directory."
  (when aide-session-status--file-watch
    (file-notify-rm-watch aide-session-status--file-watch)
    (setq aide-session-status--file-watch nil))
  (when aide-session-status--poll-timer
    (cancel-timer aide-session-status--poll-timer)
    (setq aide-session-status--poll-timer nil)))

;;;; Perspective mapping

(defun aide-session-status--persp-project-dir (persp-name)
  "Find the project directory for PERSP-NAME via Claude Code session buffers."
  (when (and (boundp 'claude-code-ide--processes)
             (hash-table-p claude-code-ide--processes))
    (let ((persp (gethash persp-name (perspectives-hash)))
          (result nil))
      (when persp
        (maphash
         (lambda (dir _process)
           (unless result
             (let ((buf-name (claude-code-ide--get-buffer-name dir)))
               (when (and buf-name
                          (let ((buf (get-buffer buf-name)))
                            (and buf (memq buf (persp-buffers persp)))))
                 (setq result dir)))))
         claude-code-ide--processes))
      result)))

;;;; Public API

(defun aide-session-status-get (persp-name)
  "Get the session status for PERSP-NAME.
Returns \"working\", \"waiting\", \"idle\", or nil if no session."
  (when-let ((dir (aide-session-status--persp-project-dir persp-name)))
    (when-let ((data (gethash (directory-file-name dir) aide-session-status--cache)))
      (alist-get 'state data))))

(defun aide-session-status-format (persp-name)
  "Format the session status for PERSP-NAME as a propertized string.
Returns a colored indicator string.  When `aide-session-status-mode' is
active but no session exists, returns a \"no session\" indicator."
  (let ((status (aide-session-status-get persp-name)))
    (if status
        (let ((face (pcase status
                      ("working" 'aide-session-status-working)
                      ("waiting" 'aide-session-status-waiting)
                      ("idle" 'aide-session-status-idle)
                      (_ 'aide-session-status-idle)))
              (label (pcase status
                       ("working" "working")
                       ("waiting" "waiting")
                       ("idle" "idle")
                       (_ status))))
          (propertize (format "● %s" label) 'face face))
      (when aide-session-status-mode
        (propertize "○ no session" 'face 'aide-session-status-none)))))

;;;; Minor mode

;;;###autoload
(define-minor-mode aide-session-status-mode
  "Global minor mode to track Claude Code session status."
  :global t
  :lighter nil
  (if aide-session-status-mode
      (aide-session-status--start-watching)
    (aide-session-status--stop-watching)
    (clrhash aide-session-status--cache)))

(provide 'aide-session-status)
;;; aide-session-status.el ends here
