;;; fingers-neo.el --- Neo mapping for fingers.el

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

;; Sample mapping from Workman to Neo.
;;
;; Example for your configuration:
;;
;; (require 'fingers)
;; (require 'fingers-neo)
;; (setq fingers-keyboard-layout-mapper 'fingers-workman-to-neo)
;; (setq fingers-region-specifiers fingers-neo-region-specifiers)
;; (fingers-reset-bindings)

;; This currently maps bindings for the main `fingers-mode-map', not for
;; `fingers-mode-x-map' or `fingers-mode-c-map' which are mostly using default
;; Emacs bindings.
;;

(defvar fingers-neo-region-specifiers
  '((char . ?z)
    (char-and-whitespace . ?Z)
    (line . ?o)
    (line-rest . ?O)
    (word . ?a)
    (word-and-whitespace . ?A)
    (symbol . ?e)
    (symbol-and-whitespace . ?E)
    (between-whitespace . ?p)
    (with-surrounding-whitespace . ?P)
    (inside-pair . ?i)
    (with-pair . ?u)
    (with-pair-and-whitespace . ?U))
  "Region specifiers tuned for the Neo keyboard layout")


(defun fingers-workman-to-neo (keys)
  (let ((workman-to-neo
	 (lambda (char)
	   (let* ((should-upcase (and
				  (or (and (>= char 65) (<= char 90))
				      (and (>= char 97) (<= char 122)))
				  (= (upcase char) char)))
		  (lower-char (downcase char))
		  (neo-char (cond ((= ?a lower-char) "u")
				  ((= ?b lower-char) "w")
				  ((= ?c lower-char) "p")
				  ((= ?d lower-char) "v")
				  ((= ?e lower-char) "r")
				  ((= ?f lower-char) "h")
				  ((= ?g lower-char) "o")
				  ((= ?h lower-char) "a")
				  ((= ?i lower-char) "d")
				  ((= ?j lower-char) "k")
				  ((= ?k lower-char) "b")
				  ((= ?l lower-char) "m")
				  ((= ?m lower-char) "ä")
				  ((= ?n lower-char) "n")
				  ((= ?o lower-char) "t")
				  ((= ?p lower-char) "f")
				  ((= ?q lower-char) "x")
				  ((= ?r lower-char) "l")
				  ((= ?s lower-char) "i")
				  ((= ?t lower-char) "e")
				  ((= ?u lower-char) "g")
				  ((= ?v lower-char) "z")
				  ((= ?w lower-char) "c")
				  ((= ?x lower-char) "ö")
				  ((= ?y lower-char) "s")
				  ((= ?z lower-char) "ü")
				  ((= ?\; lower-char) "q")
				  ((= ?\/ lower-char) "j")
				  ((= ?\[ lower-char) "ß")
				  ((= ?\' lower-char) "y")
				  (t (string lower-char)))))
	     (cond ((and should-upcase (string= ";" neo-char)) ":")
		   (should-upcase (upcase neo-char))
		   (t neo-char))))))
    (cond ((string= "SPC" keys) keys)
	  ((string= "RET" keys) keys)
	  (t
	   (apply 'concat (mapcar workman-to-neo keys))))))

(ert-deftest fingers-test:workman-to-neo ()
  (should (string= "uwpvrhoadkbmäntfxliegzcösüqjßy"
		   (fingers-workman-to-neo "abcdefghijklmnopqrstuvwxyz;/['")))
  (should (string= "uWp" (fingers-workman-to-neo "aBc")))
  (should (string= "SPC" (fingers-workman-to-neo "SPC")))
  (should (string= "RET" (fingers-workman-to-neo "RET")))
  (should (string= "y" (fingers-workman-to-neo "'")))
  (should (string= "d" (fingers-workman-to-neo "i")))
  (should (string= "q" (fingers-workman-to-neo ";")))
  (should (string= "D" (fingers-workman-to-neo "I")))
)

(provide 'fingers-neo)
