;;; aide-persp-sidebar.el --- Sidebar for perspective.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Omura Shuto

;; Author: Omura Shuto <somura-vanilla@so-icecream.com>
;; Maintainer: Omura Shuto <somura-vanilla@so-icecream.com>
;; URL: https://github.com/so-vanilla/aide
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1") (perspective "2.0"))
;; Keywords: convenience, frames

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

;; aide-persp-sidebar provides a sidebar window that displays all perspectives
;; registered in perspective.el.  The current perspective is highlighted,
;; and you can switch perspectives by clicking or pressing RET on the name.
;;
;; Features:
;; - Display all perspectives in a side window
;; - Highlight the current perspective
;; - Quick navigation between perspectives
;; - Auto-refresh when perspectives change
;; - Optional auto-show when creating new perspective
;;
;; Usage:
;;   M-x aide-persp-sidebar-show    - Show the sidebar
;;   M-x aide-persp-sidebar-toggle  - Toggle the sidebar
;;   M-x aide-persp-sidebar-close   - Close the sidebar
;;   M-x aide-persp-sidebar-focus   - Focus on the sidebar
;;
;; Keybindings in the sidebar:
;;   n, j  - Next perspective
;;   p, k  - Previous perspective
;;   RET   - Switch to perspective at point
;;   SPC   - Switch to perspective at point
;;   g     - Refresh sidebar
;;   r     - Reset sidebar width
;;   q     - Close sidebar

;;; Code:

(require 'perspective)

;; Variables
(defvar aide-persp-sidebar-buffer-name "*Aide Persp Sidebar*"
  "Name of the perspective sidebar buffer.")

(defvar aide-persp-sidebar-window nil
  "Window displaying the perspective sidebar buffer.")

(defcustom aide-persp-sidebar-auto-show-on-new t
  "Whether to automatically show sidebar when creating new perspective."
  :type 'boolean
  :group 'perspective)

;; Core functions
(defun aide-persp-sidebar--ensure-displayed ()
  "Ensure sidebar window exists and is rendered.  Does not change focus."
  (let ((buffer (get-buffer-create aide-persp-sidebar-buffer-name)))
    (with-current-buffer buffer
      (aide-persp-sidebar--render-buffer))
    (setq aide-persp-sidebar-window
          (display-buffer buffer '((display-buffer-in-side-window)
                                   (side . left)
                                   (slot . 0)
                                   (window-width . 30)
                                   (no-other-window . t))))
    aide-persp-sidebar-window))

(defun aide-persp-sidebar-show ()
  "Show perspective sidebar and focus on it."
  (interactive)
  (aide-persp-sidebar--ensure-displayed)
  (select-window aide-persp-sidebar-window))

(defun aide-persp-sidebar-toggle ()
  "Toggle perspective sidebar."
  (interactive)
  (let ((buffer (get-buffer aide-persp-sidebar-buffer-name)))
    (if (and buffer (get-buffer-window buffer))
        (aide-persp-sidebar-close)
      (aide-persp-sidebar-show))))

(defun aide-persp-sidebar-close ()
  "Close perspective sidebar."
  (interactive)
  (let ((buffer (get-buffer aide-persp-sidebar-buffer-name)))
    (when buffer
      (let ((window (get-buffer-window buffer)))
        (when window
          (delete-window window)
          (setq aide-persp-sidebar-window nil))))))

(defun aide-persp-sidebar-focus ()
  "Focus on perspective sidebar."
  (interactive)
  (let ((buffer (get-buffer aide-persp-sidebar-buffer-name)))
    (if (and buffer (get-buffer-window buffer))
        (select-window (get-buffer-window buffer))
      (aide-persp-sidebar-show))))

(defun aide-persp-sidebar-resize ()
  "Reset perspective sidebar size."
  (interactive)
  (let ((buffer (get-buffer aide-persp-sidebar-buffer-name)))
    (when buffer
      (let ((window (get-buffer-window buffer)))
        (when window
          (with-selected-window window
            (window-resize window (- 30 (window-width)) t)))))))

;; Internal functions
(defun aide-persp-sidebar--render-buffer ()
  "Render the perspective list in sidebar buffer."
  (let ((inhibit-read-only t)
        (keymap (aide-persp-sidebar--create-keymap))
        (current-persp (persp-current-name))
        (all-persps (persp-names)))
    (erase-buffer)
    (insert "aide sidebar\n")
    (insert "=============\n\n")
    (if all-persps
        (dolist (persp all-persps)
          (if (string= persp current-persp)
              ;; Highlight current perspective
              (insert (propertize (format "► %s\n" persp)
                                  'face 'highlight))
            (insert-button persp
                           'action `(lambda (button)
                                      (persp-switch ,persp))
                           'follow-link t)
            (insert "\n"))
          ;; Session status line
          (let ((status-str (and (fboundp 'aide-session-status-format)
                                 (aide-session-status-format persp))))
            (insert (if status-str
                        (format "    %s\n" status-str)
                      "\n"))))
      (insert "No perspectives\n"))
    (goto-char (point-min))
    (setq buffer-read-only t)
    (use-local-map keymap)))

(defun aide-persp-sidebar--create-keymap ()
  "Create keymap for perspective sidebar."
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "n" 'persp-next)
    (define-key keymap "j" 'persp-next)
    (define-key keymap "p" 'persp-prev)
    (define-key keymap "k" 'persp-prev)
    (define-key keymap (kbd "RET") 'aide-persp-sidebar-select-current)
    (define-key keymap (kbd "SPC") 'aide-persp-sidebar-select-current)
    (define-key keymap "q" 'aide-persp-sidebar-close)
    (define-key keymap "r" 'aide-persp-sidebar-resize)
    (define-key keymap "g" 'aide-persp-sidebar-refresh)
    keymap))

(defun aide-persp-sidebar-select-current ()
  "Select perspective at current line in sidebar buffer."
  (interactive)
  (let ((persp-name (thing-at-point 'symbol)))
    (when (and persp-name (member persp-name (persp-names)))
      (persp-switch persp-name))))

(defun aide-persp-sidebar-refresh ()
  "Refresh the sidebar content and update highlight.
When called interactively, also re-scan the session status directory."
  (interactive)
  (when (and (called-interactively-p 'any)
             (fboundp 'aide-session-status--scan-directory))
    (aide-session-status--scan-directory))
  (let ((buffer (get-buffer aide-persp-sidebar-buffer-name)))
    (when (and buffer
               aide-persp-sidebar-window
               (window-live-p aide-persp-sidebar-window))
      (save-selected-window
        (with-current-buffer buffer
          (aide-persp-sidebar--render-buffer))))))

(defun aide-persp-sidebar-on-new-perspective ()
  "Handle new perspective creation - show sidebar if enabled."
  (run-with-idle-timer 0.01 nil
                       (lambda ()
                         (if aide-persp-sidebar-auto-show-on-new
                             (aide-persp-sidebar--ensure-displayed)
                           (aide-persp-sidebar-refresh)))))

;; Auto-refresh when perspective changes
(advice-add 'persp-switch :after
            (lambda (&rest _) (aide-persp-sidebar-refresh)))

;; Phase 1: Basic perspective operation monitoring
(advice-add 'persp-new :after
            (lambda (&rest _) (aide-persp-sidebar-on-new-perspective)))

(advice-add 'persp-kill :after
            (lambda (&rest _) (aide-persp-sidebar-refresh)))

(advice-add 'persp-rename :after
            (lambda (&rest _) (aide-persp-sidebar-refresh)))

;; Phase 2: Additional perspective operation monitoring
(advice-add 'persp-next :after
            (lambda (&rest _) (aide-persp-sidebar-refresh)))

(advice-add 'persp-prev :after
            (lambda (&rest _) (aide-persp-sidebar-refresh)))

(advice-add 'persp-switch-last :after
            (lambda (&rest _) (aide-persp-sidebar-refresh)))

(advice-add 'persp-kill-others :after
            (lambda (&rest _) (aide-persp-sidebar-refresh)))

(advice-add 'persp-state-load :after
            (lambda (&rest _) (aide-persp-sidebar-refresh)))

(advice-add 'persp-state-restore :after
            (lambda (&rest _) (aide-persp-sidebar-refresh)))

(with-eval-after-load 'aide-session-status
  (add-hook 'aide-session-status--change-hook
            (lambda ()
              (run-with-idle-timer 0.1 nil #'aide-persp-sidebar-refresh))))

(provide 'aide-persp-sidebar)
;;; aide-persp-sidebar.el ends here
