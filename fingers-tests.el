;;; fingers-tests.el --- Tests for fingers.el

;; Copyright (c) 2014 Felix Geller

;; Author: Felix Geller <fgeller@gmail.com>
;; Keywords: fingers modal editing workman
;; URL: http://github.com/fgeller/fingers.el

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Tests for fingers.el

(require 'fingers)

(defun fingers-test-fingers-mark-inside-pair-strings (input position start end)
  (with-temp-buffer
    (insert input)
    (goto-char position)
    (fingers-mark-inside-pair-strings start end)
    (fingers-copy-current-region 'kill)
    (buffer-substring (point-min) (point-max))))

(defun fingers-test-fingers-mark-with-pair-strings (input position start end)
  (with-temp-buffer
    (insert input)
    (goto-char position)
    (fingers-mark-with-pair-strings start end)
    (fingers-copy-current-region 'kill)
    (buffer-substring (point-min) (point-max))))

(defun fingers-test-fingers-mark-with-pair-strings-and-whitespace (input position start end)
  (with-temp-buffer
    (insert input)
    (goto-char position)
    (fingers-mark-with-pair-strings-and-whitespace start end)
    (fingers-copy-current-region 'kill)
    (buffer-substring (point-min) (point-max))))

(ert-deftest fingers-test:fingers-mark-inside-pair ()
  (should (string= "  ()" (fingers-test-fingers-mark-inside-pair-strings "  (abc)" 5 "(" ")")))
  (should (string= "  []" (fingers-test-fingers-mark-inside-pair-strings "  [abc]" 5 "[" "]")))
  (should (string= "  ()" (fingers-test-fingers-mark-inside-pair-strings "  ((a) b (c))" 8 "(" ")")))
  (should (string= "  ''" (fingers-test-fingers-mark-inside-pair-strings "  'abc'" 5 "'" "'")))
)

(ert-deftest fingers-test:fingers-mark-with-pair ()
  (should (string= "  " (fingers-test-fingers-mark-with-pair-strings "  (abc)" 5 "(" ")")))
  (should (string= "  " (fingers-test-fingers-mark-with-pair-strings "  [abc]" 5 "[" "]")))
  (should (string= "  " (fingers-test-fingers-mark-with-pair-strings "  ((a) b (c))" 8 "(" ")")))
  (should (string= "  " (fingers-test-fingers-mark-with-pair-strings "  'abc'" 5 "'" "'")))
)

(ert-deftest fingers-test:fingers-mark-with-pair-and-whitespace ()
  (should (string= "" (fingers-test-fingers-mark-with-pair-strings-and-whitespace "  (abc)" 5 "(" ")")))
  (should (string= "" (fingers-test-fingers-mark-with-pair-strings-and-whitespace "  [abc]" 5 "[" "]")))
  (should (string= "" (fingers-test-fingers-mark-with-pair-strings-and-whitespace "  ((a) b (c))" 8 "(" ")")))
  (should (string= "" (fingers-test-fingers-mark-with-pair-strings-and-whitespace "  'abc'" 5 "'" "'")))
)
