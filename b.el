;;; b.el --- Utility functions for buffer manipulation  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Friends of Emacs-PHP development

;; Author: USAMI Kenta <tadsan@zonu.me>
;; Created: 25 Jul 2018
;; Version: 0.0.1
;; Keywords: lisp, buffer
;; Package-Requires: ((emacs "24.3") (cl-lib "0.5"))
;; URL: https://github.com/emacs-php/b.el
;; License: GPL-3.0-or-later

;; This program is free software; you can redistribute it and/or modify
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

;; This library provides generic functions for buffer manipulation and its concise API.
;; Lisp hackers who heavily abuse buffers can reduce boilerplate by this package.
;;
;; ## Functions
;;
;; - b-append(buffer string-or-buffer)
;; - b-binary?(buffer)
;; - b-blank?(buffer)
;; - b-coding-system(buffer)
;; - b-erase(buffer)
;; - b-insert(buffer &rest string-or-buffer)
;; - b-prepend(buffer string-or-buffer)
;; - b-string(buffer &key start end)
;; - b-string-with-property(buffer &key start end)
;;

;;; Code:
(require 'cl-lib)


;; Internal utility function
(defun b--point (sym-or-integer fallback)
  "Return point by `SYM-OR-INTEGER' or call `FALLBACK' function."
  (cond ((eq sym-or-integer 'point) (point))
        ((integerp sym-or-integer) sym-or-integer)
        (t (funcall fallback))))

;; Public API functions
(defun b-append (buffer string-or-buffer)
  "Insert `STRING-OR-BUFFER' to head of the `BUFFER'."
  (cl-check-type buffer buffer)
  (cl-check-type string-or-buffer (or string buffer))
  (with-current-buffer buffer
    (goto-char (point-min))
    (insert (if (bufferp string-or-buffer)
                (b-string-with-properties string-or-buffer)
              string-or-buffer)))
  buffer)

(defun b-binary? (buffer)
  "Is `BUFFER' contain binary content?"
  (declare (pure t) (side-effect-free t))
  (cl-check-type buffer buffer)
  (eq 'no-conversion (b-coding-system buffer)))

(defalias 'b-binary-p 'b-binary?)

(defun b-blank? (buffer)
  "Is `BUFFER' empty?"
  (declare (pure t) (side-effect-free t))
  (cl-check-type buffer buffer)
  (with-current-buffer buffer
    (eq (point-min) (point-max))))

(defalias 'b-blank-p 'b-blank?)

(defun b-coding-system (buffer)
  "Return `buffer-file-coding-system' by `BUFFER'."
  (declare (pure t) (side-effect-free t))
  (cl-check-type buffer buffer)
  (with-current-buffer buffer
    buffer-file-coding-system))

(defun b-erase (buffer)
  "Delete the entire contents of the `BUFFER'."
  (cl-check-type buffer buffer)
  (with-current-buffer buffer
    (erase-buffer))
  buffer)

(defun b-insert (buffer &rest string-or-buffer)
  "Insert `STRING-OR-BUFFER' to the `BUFFER'."
  (cl-check-type buffer buffer)
  (with-current-buffer buffer
    (cl-loop for s-or-b in string-or-buffer
             do (insert (if (bufferp s-or-b)
                            (b-string-with-properties s-or-b)
                          s-or-b))))
  buffer)

(defun b-prepend (buffer string-or-buffer)
  "Insert `STRING-OR-BUFFER' to bottom of the `BUFFER'."
  (cl-check-type buffer buffer)
  (cl-check-type string-or-buffer (or string buffer))
  (with-current-buffer buffer
    (goto-char (point-max))
    (insert (if (bufferp string-or-buffer)
                (b-string-with-properties string-or-buffer)
              string-or-buffer)))
  buffer)

(cl-defun b-string (buffer &key start end)
  "Return the contents of part of the `BUFFER', without the text properties."
  (declare (pure t) (side-effect-free t))
  (cl-check-type buffer buffer)
  (cl-assert (or (null start) (eq 'point start) (integerp start)))
  (cl-assert (or (null end) (eq 'point end) (integerp end)))
  (with-current-buffer buffer
    (buffer-substring-no-properties (b--point start 'point-min)
                                    (b--point end 'point-max))))

;; `buffer-string' and `buffer-substring' have no effect on shorthand.
;; On the other hand, `buffer-substring-no-properties' is cumbersome.
;; So the `b-string' name is his. ヾ(〃＞＜)ﾉﾞ
(defalias 'b-string-no-properties 'b-string)

(cl-defun b-string-with-properties (buffer &key start end)
  "Return the contents of part of the `BUFFER' as a string."
  (declare (pure t) (side-effect-free t))
  (cl-check-type buffer buffer)
  (cl-assert (or (null start) (eq 'point start) (integerp start)))
  (cl-assert (or (null end) (eq 'point end) (integerp end)))
  (with-current-buffer buffer
    (buffer-substring (b--point start 'point-min)
                      (b--point end 'point-max))))

(provide 'b)
;;; b.el ends here
