;;; cangjie-practice.el --- Cangjie practice session -*- lexical-binding: t -*-

;; Author: Kisaragi Hiu
;; Keywords: keywords


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; Practice Cangjie by typing articles of your choosing with Cangjie
;; code prompted in the echo area.

;;; Code:

(require 'cangjie)
(require 'dash)

(defvar cangjie-practice--format-values)

(defun cangjie-practice-cycle-format ()
  "Cycle `cangjie-format'."
  (interactive)
  (let* ((value-pairs (->> (cdr (get 'cangjie-display-format 'custom-type))
                           (--map (cons (car (last it))
                                        (plist-get (cdr it) :tag)))))
         (vals (-map #'car value-pairs))
         (val-names (-map #'cdr value-pairs))
         (position (or (cl-position cangjie-display-format vals) 0))
         (new-position (mod (1+ position) (length value-pairs)))
         (new-value (elt vals new-position))
         (new-name (elt val-names new-position)))
    (setq cangjie-display-format new-value)
    (message "Cangjie display format: %s" new-name)))

(define-derived-mode cangjie-practice-mode text-mode "Cangjie-Practice"
  "Cangjie Practice mode.

Do not start this major mode directly; use `cangjie-practice'
instead."
  (unless (eq this-command 'cangjie-practice)
    (error "Use cangjie-practice instead of starting this mode directly"))
  (define-key cangjie-practice-mode-map
    (kbd "C-c C-c") #'cangjie-practice-cycle-format))

;;;###autoload
(defun cangjie-practice ()
  "Start a Cangjie practice session.)))

Should be run in a buffer containing the text to practice.

This will discard the current window layout; launch in a new
frame if you don't want that to happen.

The Cangjie code for the next character to type will be displayed
in the echo area while you type. Use `cangjie-display-format' to control
the way it's displayed.

\\{cangjie-practice-mode-map}"
  (interactive)
  (delete-other-windows)
  (let ((text-buf (current-buffer))
        (practice-buf (pop-to-buffer (get-buffer-create "*Cangjie Practice*"))))
    (with-current-buffer practice-buf
      (delete-region (point-min) (point-max))
      (cangjie-practice-mode)
      (add-hook
       'post-command-hook
       (lambda () (cangjie--at-point-in-buf text-buf))
       nil
       :local))))

(provide 'cangjie-practice)

;;; cangjie-practice.el ends here
