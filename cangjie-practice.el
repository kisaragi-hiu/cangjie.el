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

;; A Cangjie typing practice mode.

;;; Code:

(require 'cangjie)
(require 'dash)
(require 's)

(defun cangjie-practice--trim (string)
  "Trim STRING, treating IDEOGRAPHIC SPACE (　) as a whitespace as well."
  (string-trim string "[ \t\n\r　]+" "[ \t\n\r　]+"))

(defvar-local cangjie-practice--state nil)

(defmacro cangjie-practice--state (key)
  "Access KEY in `cangjie-practice--state'."
  `(map-elt cangjie-practice--state ,key))

(defun cangjie-practice--state-current-line ()
  "Return current line as a string."
  (elt (cangjie-practice--state :lines)
       (cangjie-practice--state :line-index)))

(defun cangjie-practice--state-current-char ()
  "Return current char as a string."
  (condition-case _
      (-some-> (cangjie-practice--state-current-line)
        (elt (cangjie-practice--state :char-index))
        string)
    (args-out-of-range nil)))

(defun cangjie-practice--load-file (file)
  "Load FILE."
  (setf
   (cangjie-practice--state :line-index) 0
   (cangjie-practice--state :char-index) 0
   (cangjie-practice--state :file-name) file
   (cangjie-practice--state :lines)
   (->> (f-read-text file)
     s-trim
     (s-split "\n"))))

(cl-defun cangjie-practice--increment-line (&optional (n 1))
  "Increment `:line-index', skipping through empty lines.

Also resets `:char-index' to 0.

If N is non-nil, increment N lines at a time. A value of -1 can
be used for decrementing."
  (cl-incf (cangjie-practice--state :line-index) n)
  (setf (cangjie-practice--state :char-index) 0)
  (while (s-matches? "^[ \t\n\r　]*$" (cangjie-practice--state-current-line))
    (cl-incf (cangjie-practice--state :line-index) n)))

(cl-defun cangjie-practice--increment-char (&optional (n 1))
  "Increment `:char-index', skipping through non-han characters.

If we finish a line, continue onto the next line.

If N is non-nil, increment N characters at a time. A value of -1
can be used for decrementing."
  (cl-incf (cangjie-practice--state :char-index) n)
  (cl-block nil
    (while t
      (cond
       ((not (cangjie-practice--state-current-char))
        (cangjie-practice--increment-line))
       ((cangjie (cangjie-practice--state-current-char))
        (cl-return))
       (t
        (cl-incf (cangjie-practice--state :char-index) n))))))

(defmacro cangjie-practice--widget (name &rest body)
  "Set up widget content update for NAME and run BODY."
  (declare (indent 1))
  (let ((start-key (intern (concat ":" name "-start")))
        (end-key (intern (concat ":" name "-end"))))
    `(save-excursion
       (-when-let* ((start (cangjie-practice--state ,start-key))
                    (end (cangjie-practice--state ,end-key)))
         (goto-char start)
         (delete-region start end))
       (setf (cangjie-practice--state ,start-key) (point))
       ,@body
       (setf (cangjie-practice--state ,end-key) (point))
       (- (cangjie-practice--state ,end-key) (cangjie-practice--state ,start-key)))))

(defun cangjie-practice--widget-current-char ()
  "Insert the current character and its Cangjie code."
  (let ((character (cangjie-practice--state-current-char)))
    (insert character "\n\n"
            (cangjie character) "\n"
            (cangjie--han-to-abc
             (cangjie character))
            "\n\n")))

(defun cangjie-practice--widget-current-line ()
  "Insert the current line."
  (cangjie-practice--widget "current-line"
    (insert (cangjie-practice--trim (cangjie-practice--state-current-line))
            "\n\n")
    (cangjie-practice--widget-current-char)))

(defun cangjie-practice--widget-input-area ()
  "."
  (insert "練習區域\n\n"))


(cl-defun cangjie-practice--post-command ()
  "Post-command hook."
  (unless (eq this-command #'self-insert-command)
    (cl-return-from cangjie-practice--post-command))
  (let ((inserted-char (string last-command-event)))
    (when (equal (cangjie-practice--state-current-char) inserted-char)
      (cangjie-practice--increment-char)
      (cangjie-practice--widget-current-line))))

(defun cangjie-practice--view-init ()
  "."
  (erase-buffer)
  (setq cangjie-practice--state nil)
  (insert "倉頡練習\n\n")
  (insert-text-button
   "[載入文本]"
   'follow-link t
   'action (lambda (&rest _)
             (and (cangjie-practice--load-file (read-file-name "文本："))
                  (cangjie-practice--view-loaded)))))

(defun cangjie-practice--view-loaded ()
  "."
  (erase-buffer)
  (insert "倉頡練習：" (map-elt cangjie-practice--state :file-name) " ")
  (insert-text-button
   "[載入文本]"
   'follow-link t
   'action (lambda (&rest _)
             (and (cangjie-practice--load-file
                   (read-file-name "文本："))
                  (cangjie-practice--view-loaded))))
  (insert "\n\n")
  (forward-char (cangjie-practice--widget-current-line))
  (cangjie-practice--widget-current-char)
  (cangjie-practice--widget-input-area))

;;;###autoload
(defun cangjie-practice ()
  "Start a Cangjie practice session."
  (interactive)
  (let ((buf (pop-to-buffer-same-window
              (get-buffer-create "*Cangjie Practice*"))))
    (with-current-buffer buf
      (cangjie-practice--view-init)
      (add-hook 'post-command-hook #'cangjie-practice--post-command nil t))))

(provide 'cangjie-practice)

;;; cangjie-practice.el ends here
