;;; smart-delete.el --- IntelliJ-like backspace/delete

;; Copyright (C) 2020 Leonardo Dagnino

;; Author: Leonardo Schripsema
;; Created: 2020-06-13
;; Version: 0.1.0
;; Keywords: delete, backspace
;; URL: https://github.com/leodag/smart-delete

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Handle backspace/delete like IntelliJ IDEs.
;; Does a simple deletion when:
;; * Called with an argument
;; * Inside a string literal
;; * Have non-spacing characters before/after point, respectively

;; Deletes region when region is active and delete-active-region
;; is non-nil.

;; Otherwise does a smart delete:

;; When smart deleting backward:
;; If after indentation level, go back to indentation
;; If at or before indentation level, delete to end of previous
;; line

;; When smart deleting forward:
;; Delete spaces after point
;; Smart delete backward at next line

;; Some ideas taken from https://github.com/itome/smart-backspace
;; Most of the code was taken from `delete-forward-char' and
;; `delete-backward-char'

;; Usage:

;; (global-set-key (kbd "<backspace>") 'smart-delete-backward)
;; (global-set-key (kbd "<delete>") 'smart-delete-forward)

;;; Code:

(defun smart-delete-only-indentation-before-point ()
  (save-excursion
    (let ((skip (skip-chars-backward " \t")))
      (if (bolp)
          skip))))

(defun smart-delete-only-spaces-after-point ()
  (save-excursion
    (let ((skip (skip-chars-forward " ")))
      (if (eolp)
          skip))))

(defvar smart-delete-simple-forward-function 'delete-forward-char
  "Function to call when deleting forward and not using smart
deletion.")

(defvar smart-delete-simple-backward-function 'backward-delete-char-untabify
  "Function to call when deleting backward and not using smart
deletion.")

(defun smart-delete-backward (n &optional killflag)
  (interactive "*p\nP")
  (cond
   ;; With an argument, simple delete
   ((not (= n 1))
    (funcall smart-delete-simple-backward-function n killflag))
   ;; If a region is active, kill or delete it.
   ((and (use-region-p)
         delete-active-region)
    (if (eq delete-active-region 'kill)
        (kill-region (region-beginning) (region-end) 'region)
      (funcall region-extract-function 'delete-only)))
   ;; If inside a string literal, simple deletion
   ((nth 3 (syntax-ppss))
    (funcall smart-delete-simple-backward-function n killflag))
   ;; If only indentation before point, smart backspace
   (#1=(smart-delete-only-indentation-before-point)
       (let* ((indentation #1#)
              (current-col (current-column))
              (indent-offset (progn
                               (indent-according-to-mode)
                               (- (current-column) current-col))))
         (when (>= indent-offset 0)
           (delete-char (+ indentation (- indent-offset) 1) killflag)
           (when (smart-delete-only-indentation-before-point)
             (indent-according-to-mode)))))
   ;; Otherwise, do simple deletion.
   (t
    (funcall smart-delete-simple-backward-function n killflag))))

(defun smart-delete-forward (n &optional killflag)
  (interactive "*p\nP")
  (cond
   ;; With an argument, simple delete
   ((not (= n 1))
    (funcall smart-delete-simple-forward-function n killflag))
   ;; If a region is active, kill or delete it.
   ((and (use-region-p)
         delete-active-region)
    (if (eq delete-active-region 'kill)
        (kill-region (region-beginning) (region-end) 'region)
      (funcall region-extract-function 'delete-only)))
   ;; If inside a string literal, simple deletion
   ((nth 3 (syntax-ppss))
    (funcall smart-delete-simple-forward-function n killflag))
   ;; If only indentation before point, smart delete
   (#1=(smart-delete-only-spaces-after-point)
       (delete-char #1# killflag)
       (let ((pre-column (current-column)))
         (forward-line 1)
         (smart-delete-backward n killflag)
         (let ((offset (- pre-column (current-column))))
           (message "%s" offset)
           (when (> offset 0)
             (insert-char ?\s offset)))))
   ;; Otherwise, do simple deletion.
   (t
    (funcall smart-delete-simple-forward-function n killflag))))

(provide 'smart-delete)

;;; smart-delete.el ends here
