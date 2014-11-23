;; Sample mapping from Workman to Qwerty.
;;
;; Example for your configuration:
;;
;; (require 'fingers)
;; (require 'fingers-qwerty)
;; (setq fingers-keyboard-layout-mapper 'fingers-workman-to-qwerty)
;; (fingers-reset-bindings)
;;
;; (setq fingers-selection-specifiers
;;   '((line . ?f)
;;     (line-rest . ?r)
;;     (word . ?d)
;;     (symbol . ?e)
;;     (inside-pair . ?s)
;;     (with-pair . ?a)
;;     (with-pair-and-whitespace . ?q)))
;;

;; This currently maps bindings for the main `fingers-mode-map', not for
;; `fingers-mode-x-map' or `fingers-mode-c-map' which are mostly using default
;; Emacs bindings.
;;
;; Bindings are printed to the *Messages* buffer for debug info.
;;

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
				     ((= ?c lower-char) "v")
				     ((= ?d lower-char) "w")
				     ((= ?e lower-char) "k")
				     ((= ?f lower-char) "u")
				     ((= ?g lower-char) "g")
				     ((= ?h lower-char) "d")
				     ((= ?i lower-char) ";")
				     ((= ?j lower-char) "y")
				     ((= ?k lower-char) "n")
				     ((= ?l lower-char) "m")
				     ((= ?m lower-char) "c")
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

(ert-deftest fingers-test:workman-to-qwerty ()
  (should (string= "atvwkugd;ynmcjloqesfibrxhzp" (fingers-workman-to-qwerty "abcdefghijklmnopqrstuvwxyz;")))
  (should (string= "aTv" (fingers-workman-to-qwerty "aBc")))
  (should (string= "SPC" (fingers-workman-to-qwerty "SPC")))
  (should (string= "RET" (fingers-workman-to-qwerty "RET")))
  (should (string= "'" (fingers-workman-to-qwerty "'")))
  (should (string= ";" (fingers-workman-to-qwerty "i")))
  (should (string= "p" (fingers-workman-to-qwerty ";")))
  (should (string= ":" (fingers-workman-to-qwerty "I")))
)

(provide 'fingers-qwerty)
