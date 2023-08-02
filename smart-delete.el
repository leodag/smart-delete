;;; smart-delete.el --- IntelliJ-like backspace/delete -*- lexical-binding: t -*-

;; Copyright (C) 2020 Leonardo Dagnino

;; Author: Leonardo Schripsema
;; Created: 2020-06-13
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.1"))
;; Keywords: emulations, wp
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

;; When `smart-delete-mode' is enabled, do a smart delete, unless:
;; * Called with an argument
;; * Inside a string literal
;; * Have non-spacing characters before/after point (for
;; backward/forward deletes, respectively)

;; Deletes region when region is active and delete-active-region
;; is non-nil.

;; A smart delete consists of:

;; When smart deleting backward:
;; If after indentation level, go back to indentation
;; If at or before indentation level, delete to end of previous
;; line

;; When smart deleting forward:
;; Delete spaces after point
;; Smart delete backward at next line

;; Usage:

;; (smart-delete-mode 1)

;; Some ideas taken from https://github.com/itome/smart-backspace

;;; Code:

(defun smart-delete--only-space-before-point ()
  "Nil if there is a non-space character before point. Returns
the distance traveled if there isn't."
  (save-excursion
    ;; LIM is needed in case \n has " " syntax (as in SML mode)
    (let ((skip (skip-syntax-backward " " (line-beginning-position))))
      (if (bolp)
          skip))))

(defun smart-delete--only-space-after-point ()
  "Nil if there is a non-space character after point. Returns the
distance traveled if there isn't."
  (save-excursion
    (let ((skip (skip-syntax-forward " " (line-end-position))))
      (if (eolp)
          skip))))

(defun smart-delete-backward (n &optional killflag)
  "Does a smart delete backwards. Does not check for the
conditions on whether a smart delete should be run - that is left
to the keymap's filter. So don't bind this to a key."
  (interactive "p\nP")
  ;; must operate relative to point - if you only consider
  ;; columns, there will be problems when indent-tabs-mode
  (let* ((indentation (smart-delete--only-space-before-point))
         (current-point (point))
         (indent-offset (progn
                          (indent-according-to-mode)
                          (- (point) current-point))))
    (when (>= indent-offset 0)
      (delete-char (- indentation indent-offset 1) killflag)
      (when (smart-delete--only-space-before-point)
        (indent-according-to-mode)))))

(defun smart-delete-forward (n &optional killflag)
  "Does a smart delete forward. Does not check for the
conditions on whether a smart delete should be run - that is left
to the keymap's filter. So don't bind this to a key."
  (interactive "p\nP")
  (delete-char (smart-delete--only-space-after-point) killflag)
  (let ((pre-column (current-column)))
    (forward-char 1)
    (smart-delete-backward n killflag)
    (let ((offset (- pre-column (current-column))))
      (when (> offset 0)
        (insert-char ?\s offset)))))

(defun smart-delete--should-smart-delete-backward-filter (cmd)
  "Filter function to be used in a keymap to decide whether a
  smart delete should be run."
  (and (not prefix-arg)
       (not (and (use-region-p)
                 delete-active-region))
       (not (nth 3 (syntax-ppss)))
       (smart-delete--only-space-before-point)
       cmd))

(defun smart-delete--should-smart-delete-forward-filter (cmd)
  "Filter function to be used in a keymap to decide whether a
  smart delete should be run."
  (and (not prefix-arg)
       (not (and (use-region-p)
                 delete-active-region))
       (not (nth 3 (syntax-ppss)))
       (smart-delete--only-space-after-point)
       cmd))

(defvar smart-delete-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "DEL")
      `(menu-item
        "" smart-delete-backward
        :filter smart-delete--should-smart-delete-backward-filter))
    (define-key map (kbd "<deletechar>")
      `(menu-item
        "" smart-delete-forward
        :filter smart-delete--should-smart-delete-forward-filter))
    map))

;;;###autoload
(define-minor-mode smart-delete-mode
  "Does a smart delete when deleting forward in a line where
  there is only space after point and when deleting backward when
  there is only space before point.")

(provide 'smart-delete)

;;; smart-delete.el ends here
