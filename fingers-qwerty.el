;;; fingers-qwerty.el --- Qwerty mapping for fingers.el

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

;; Sample mapping from Workman to Qwerty.
;;
;; Example for your configuration:
;;
;; (require 'fingers)
;; (require 'fingers-qwerty)
;; (setq fingers-keyboard-layout-mapper 'fingers-workman-to-qwerty)
;; (setq fingers-region-specifiers fingers-qwerty-region-specifiers)
;; (fingers-reset-bindings)

;; This currently maps bindings for the main `fingers-mode-map', not for
;; `fingers-mode-x-map' or `fingers-mode-c-map' which are mostly using default
;; Emacs bindings.
;;
;; Bindings are printed to the *Messages* buffer for debug info.
;;

(defvar fingers-qwerty-region-specifiers
  '((char . ?b)
    (char-and-whitespace . ?B)
    (line . ?g)
    (line-rest . ?G)
    (word . ?d)
    (word-and-whitespace . ?D)
    (symbol . ?f)
    (symbol-and-whitespace . ?F)
    (between-whitespace . ?v)
    (with-surrounding-whitespace . ?V)
    (inside-pair . ?s)
    (with-pair . ?a)
    (with-pair-and-whitespace . ?A))
  "Region specifiers tuned for the Qwerty layout.")

(defun fingers-workman-to-qwerty (keys)
  (let ((workman-to-qwerty
	 (lambda (char)
	   (let* ((should-upcase (and
				  (or (and (>= char 65) (<= char 90))
				      (and (>= char 97) (<= char 122)))
				  (= (upcase char) char)))
		  (lower-char (downcase char))
		  (qwerty-char (cond ((= ?a lower-char) "a")
				     ((= ?b lower-char) "t")
				     ((= ?c lower-char) "c")
				     ((= ?d lower-char) "w")
				     ((= ?e lower-char) "k")
				     ((= ?f lower-char) "u")
				     ((= ?g lower-char) "g")
				     ((= ?h lower-char) "d")
				     ((= ?i lower-char) ";")
				     ((= ?j lower-char) "y")
				     ((= ?k lower-char) "n")
				     ((= ?l lower-char) "m")
				     ((= ?m lower-char) "v")
				     ((= ?n lower-char) "j")
				     ((= ?o lower-char) "l")
				     ((= ?p lower-char) "o")
				     ((= ?q lower-char) "q")
				     ((= ?r lower-char) "e")
				     ((= ?s lower-char) "s")
				     ((= ?t lower-char) "f")
				     ((= ?u lower-char) "i")
				     ((= ?v lower-char) "b")
				     ((= ?w lower-char) "r")
				     ((= ?x lower-char) "x")
				     ((= ?y lower-char) "h")
				     ((= ?z lower-char) "z")
				     ((= ?\; lower-char) "p")
				     (t (string lower-char)))))
	     (cond ((and should-upcase (string= ";" qwerty-char)) ":")
		   (should-upcase (upcase qwerty-char))
		   (t qwerty-char))))))
    (cond ((string= "SPC" keys) keys)
	  ((string= "RET" keys) keys)
	  (t
	   (apply 'concat (mapcar workman-to-qwerty keys))))))

(provide 'fingers-qwerty)
