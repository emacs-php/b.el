;;; b.el --- Utility functions for buffer manipulation  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Friends of Emacs-PHP development

;; Author: USAMI Kenta <tadsan@zonu.me>
;;         Dominik Honnef
;; Created: 25 Jul 2018
;; Version: 0.0.1
;; Keywords: lisp, buffer
;; Package-Requires: ((emacs "24.3"))
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
;; - b-apply-rcs-patch(target-buffer patch-buffer)
;; - b-binary?(buffer)
;; - b-blank?(buffer)
;; - b-bytes-length(buffer)
;; - b-bytes-position(buffer)
;; - b-coding-system(buffer)
;; - b-duplicate(buffer)
;; - b-erase(buffer)
;; - b-insert(buffer &rest string-or-buffer)
;; - b-length(buffer)
;; - b-move(buffer position &key bytes)
;; - b-move-backward(buffer backward-pos &key bytes)
;; - b-move-bytes(buffer position)
;; - b-move-forward(buffer forward-pos &key bytes)
;; - b-position(buffer)
;; - b-prepend(buffer string-or-buffer)
;; - b-string(buffer &key start end)
;; - b-string-with-property(buffer &key start end)
;;

;;; Code:
(require 'cl-lib)

;; Constants
(defconst b-point-keywords
  '(point
    point-1
    point+1))

;; Internal utility function
(defun b--point (sym-or-integer fallback)
  "Return point by SYM-OR-INTEGER or call FALLBACK function."
  (cond ((eq sym-or-integer 'point) (point))
        ((eq sym-or-integer 'point-1) (1- (point)))
        ((eq sym-or-integer 'point+1) (1+ (point)))
        ((integerp sym-or-integer) sym-or-integer)
        (t (funcall fallback))))

;; Public API functions
(defun b-append (buffer string-or-buffer)
  "Insert STRING-OR-BUFFER to bottom of the BUFFER."
  (cl-check-type buffer buffer)
  (cl-check-type string-or-buffer (or string buffer))
  (with-current-buffer buffer
    (goto-char (point-max))
    (insert (if (bufferp string-or-buffer)
                (b-string-with-properties string-or-buffer)
              string-or-buffer)))
  buffer)

(defun b-binary? (buffer)
  "Is BUFFER contain binary content?"
  (declare (pure t) (side-effect-free t))
  (cl-check-type buffer buffer)
  (eq 'no-conversion (b-coding-system buffer)))

(defalias 'b-binary-p 'b-binary?)

(defun b-blank? (buffer)
  "Is BUFFER empty?"
  (declare (pure t) (side-effect-free t))
  (cl-check-type buffer buffer)
  (with-current-buffer buffer
    (eq (point-min) (point-max))))

(defalias 'b-blank-p 'b-blank?)

(defun b-bytes-length (buffer)
  "Return bytes length of BUFFER."
  (cl-check-type buffer buffer)
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (position-bytes (point-max)))))

(defun b-bytes-position (buffer)
  "Return bytes position of cursor point in BUFFER."
  (cl-check-type buffer buffer)
  (with-current-buffer buffer
    (position-bytes (point))))

(defun b-coding-system (buffer)
  "Return `buffer-file-coding-system' by BUFFER."
  (declare (pure t) (side-effect-free t))
  (cl-check-type buffer buffer)
  (with-current-buffer buffer
    buffer-file-coding-system))

(cl-defun b-duplicate (buffer target-buffer &key start end)
  "Duplicate the contents of BUFFER from START to END into TARGET-BUFFER."
  (declare (pure t) (side-effect-free t))
  (cl-check-type buffer buffer)
  (cl-assert (or (null start) (memq start b-point-keywords) (integerp start)))
  (cl-assert (or (null end) (memq end b-point-keywords) (integerp end)))
  (with-current-buffer target-buffer
    (insert (b-string buffer :start start :end end)))
  target-buffer)

(defun b-erase (buffer)
  "Delete the entire contents of the BUFFER."
  (cl-check-type buffer buffer)
  (with-current-buffer buffer
    (erase-buffer))
  buffer)

(defun b-insert (buffer &rest string-or-buffer)
  "Insert STRING-OR-BUFFER to the BUFFER."
  (cl-check-type buffer buffer)
  (with-current-buffer buffer
    (cl-loop for s-or-b in string-or-buffer
             do (insert (if (bufferp s-or-b)
                            (b-string-with-properties s-or-b)
                          s-or-b))))
  buffer)

(defun b-length (buffer)
  "Return chars length of BUFFER."
  (cl-check-type buffer buffer)
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (point-max))))

(cl-defun b-move (buffer position &key bytes)
  "Set point cursor to POSITION in BUFFER."
  (cl-check-type buffer buffer)
  (cl-check-type position integer)
  (cl-check-type bytes boolean)
  (with-current-buffer buffer
    (goto-char (if bytes (position-bytes position) position)))
  buffer)

(cl-defun b-move-backward (buffer backward-pos &key bytes)
  "Set point cursor backward BACKWARD-POS from current point in BUFFER."
  (b-move-backward buffer (* -1 backward-pos) :bytes bytes))

(defun b-move-bytes (buffer position)
  "Set point cursor to POSITION in BUFFER."
  (b-move buffer position :bytes t))

(cl-defun b-move-forward (buffer forward-pos &key bytes)
  "Set point cursor forward FORWARD-POS from current point in BUFFER."
  (cl-check-type buffer buffer)
  (cl-check-type forward-pos integer)
  (cl-check-type bytes boolean)
  (with-current-buffer buffer
    (let ((position (+ forward-pos (point))))
      (goto-char (if bytes (position-bytes position) position))))
  buffer)

(defun b-position (buffer)
  "Return position of cursor point in BUFFER."
  (cl-check-type buffer buffer)
  (with-current-buffer buffer
    (point)))

(defun b-prepend (buffer string-or-buffer)
  "Insert STRING-OR-BUFFER to head of the BUFFER."
  (cl-check-type buffer buffer)
  (cl-check-type string-or-buffer (or string buffer))
  (with-current-buffer buffer
    (goto-char (point-min))
    (insert (if (bufferp string-or-buffer)
                (b-string-with-properties string-or-buffer)
              string-or-buffer)))
  buffer)

(cl-defun b-string (buffer &key start end)
  "Return the contents of BUFFER from START to END, without any text properties."
  (declare (pure t) (side-effect-free t))
  (cl-check-type buffer buffer)
  (cl-assert (or (null start) (memq start b-point-keywords) (integerp start)))
  (cl-assert (or (null end) (memq end b-point-keywords) (integerp end)))
  (with-current-buffer buffer
    (buffer-substring-no-properties (b--point start 'point-min)
                                    (b--point end 'point-max))))

;; `buffer-string' and `buffer-substring' have no effect on shorthand.
;; On the other hand, `buffer-substring-no-properties' is cumbersome.
;; So the `b-string' name is his. ヾ(〃＞＜)ﾉﾞ
(defalias 'b-string-no-properties 'b-string)

(cl-defun b-string-with-properties (buffer &key start end)
  "Return the contents of BUFFER from START to END as a string."
  (declare (pure t) (side-effect-free t))
  (cl-check-type buffer buffer)
  (cl-assert (or (null start) (memq start b-point-keywords) (integerp start)))
  (cl-assert (or (null end) (memq end b-point-keywords) (integerp end)))
  (with-current-buffer buffer
    (buffer-substring (b--point start 'point-min)
                      (b--point end 'point-max))))


;; gofmt apply-rcs-patch Function
;; These functions are copied by go-mode(gofmt).
(defun b--delete-whole-line (&optional arg)
  "Delete the current line without putting it in the `kill-ring'.
Derived from function `kill-whole-line'.  ARG is defined as for that
function."
  (setq arg (or arg 1))
  (if (and (> arg 0)
           (eobp)
           (save-excursion (forward-visible-line 0) (eobp)))
      (signal 'end-of-buffer nil))
  (if (and (< arg 0)
           (bobp)
           (save-excursion (end-of-visible-line) (bobp)))
      (signal 'beginning-of-buffer nil))
  (cond ((zerop arg)
         (delete-region (progn (forward-visible-line 0) (point))
                        (progn (end-of-visible-line) (point))))
        ((< arg 0)
         (delete-region (progn (end-of-visible-line) (point))
                        (progn (forward-visible-line (1+ arg))
                               (unless (bobp)
                                 (backward-char))
                               (point))))
        (t
         (delete-region (progn (forward-visible-line 0) (point))
                        (progn (forward-visible-line arg) (point))))))

(defun b-apply-rcs-patch (target-buffer patch-buffer)
  "Apply an RCS-formatted diff from PATCH-BUFFER to the TARGET-BUFFER."
  (let (
        ;; Relative offset between buffer line numbers and line numbers
        ;; in patch.
        ;;
        ;; Line numbers in the patch are based on the source file, so
        ;; we have to keep an offset when making changes to the
        ;; buffer.
        ;;
        ;; Appending lines decrements the offset (possibly making it
        ;; negative), deleting lines increments it. This order
        ;; simplifies the forward-line invocations.
        (line-offset 0)
        (column (current-column)))
    (save-excursion
      (with-current-buffer patch-buffer
        (goto-char (point-min))
        (while (not (eobp))
          (unless (looking-at "^\\([ad]\\)\\([0-9]+\\) \\([0-9]+\\)")
            (error "Invalid rcs patch or internal error in b-apply-rcs-patch"))
          (forward-line)
          (let ((action (match-string 1))
                (from (string-to-number (match-string 2)))
                (len  (string-to-number (match-string 3))))
            (cond
             ((equal action "a")
              (let ((start (point)))
                (forward-line len)
                (let ((text (buffer-substring start (point))))
                  (with-current-buffer target-buffer
                    (cl-decf line-offset len)
                    (goto-char (point-min))
                    (forward-line (- from len line-offset))
                    (insert text)))))
             ((equal action "d")
              (with-current-buffer target-buffer
                (goto-char (point-min))
                (forward-line (1- (- from line-offset)))
                (cl-incf line-offset len)
                (b--delete-whole-line len)))
             (t
              (error "Invalid rcs patch or internal error in b--apply-rcs-patch")))))))
    (move-to-column column)))
;; Copy of go-mode.el ends here

(provide 'b)
;;; b.el ends here
