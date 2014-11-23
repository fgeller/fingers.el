;;
;; tests
;;
(require 'fingers)

(defun fingers-test-fingers-mark-inside-pair-strings (input position start end)
  (with-temp-buffer
    (insert input)
    (goto-char position)
    (fingers-mark-inside-pair-strings start end)
    (fingers-kill-current-region)
    (buffer-substring (point-min) (point-max))))

(defun fingers-test-fingers-mark-with-pair-strings (input position start end)
  (with-temp-buffer
    (insert input)
    (goto-char position)
    (fingers-mark-with-pair-strings start end)
    (fingers-kill-current-region)
    (buffer-substring (point-min) (point-max))))

(defun fingers-test-fingers-mark-with-pair-strings-and-whitespace (input position start end)
  (with-temp-buffer
    (insert input)
    (goto-char position)
    (fingers-mark-with-pair-strings-and-whitespace start end)
    (fingers-kill-current-region)
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
