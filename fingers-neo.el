;; Sample mapping from Workman to Neo.
;;
;; Example for your configuration:
;;
;; (require 'fingers)
;; (require 'fingers-neo)
;; (setq fingers-keyboard-layout-mapper 'fingers-workman-to-neo)
;; (fingers-reset-bindings)
;;
;; (setq fingers-selection-specifiers
;;   '((char . ?z)
;;     (char-and-whitespace . ?Z)
;;     (line . ?e)
;;     (line-rest . ?c)
;;     (word . ?a)
;;     (word-and-whitespace . ?A)
;;     (symbol . ?l)
;;     (symbol-and-whitespace . ?L)
;;     (inside-pair . ?i)
;;     (with-pair . ?u)
;;     (with-pair-and-whitespace . ?U)))
;;

;; This currently maps bindings for the main `fingers-mode-map', not for
;; `fingers-mode-x-map' or `fingers-mode-c-map' which are mostly using default
;; Emacs bindings.
;;

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
