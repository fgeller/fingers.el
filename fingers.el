;;; fingers.el --- Modal editing.

;; Author: Felix Geller <fgeller@gmail.com>
;; Keywords: fingers modal editing workman
;;
;; Requires `thingatpt' to identify symbol at point.
;;
;; Based on ideas found in `boon' and `god-mode'.
;;  - boon: https://github.com/jyp/boon
;;  - god-mode: https://github.com/chrisdone/god-mode
;;
;; TODOs:
;;  - Use SubWord mode for marking word
;;  - Register as global minor mode?
;;  - Remove redundant jump for next occurrence helpers
;;

(require 'thingatpt)

(defun fingers-mode-clean-map ()
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    map))

(defvar fingers-mode-map (fingers-mode-clean-map))
(defvar fingers-mode-x-map (fingers-mode-clean-map))
(defvar fingers-mode-c-map (fingers-mode-clean-map))

;;
;; Helpers for bindings
;;
(defun fingers-pass-events (kbd-string)
  "Helper to pass keyboard events through to shadowed maps. Based on `boon-push-events'"
  (setq unread-command-events
        (append (kbd kbd-string) unread-command-events)))

(defmacro fingers-pass-events-command (kbd-string)
  `(lambda ()
     (interactive)
     (fingers-pass-events ,kbd-string)))

(defun define-keys (map bindings)
  "Defines bindings in MAP as defined in BINDINGS"
  (dolist (binding bindings)
    (let ((key (cond ((symbolp (car binding)) (symbol-name (car binding)))
                     ((numberp (car binding)) (number-to-string (car binding)))
                     (t (error (format "unexpected key: %s" (car binding))))))
          (target (cdr binding)))
      (message "Defining binding for [%s] to target [%s]" key target)
      (define-key map (kbd key) target))))

;;
;; Helpers for navigation
;;
(defun fingers-move-to-next-word-occurrence ()
  (interactive)
  (search-forward (thing-at-point 'word)))

(defun fingers-move-to-next-symbol-occurrence ()
  (interactive)
  (search-forward (thing-at-point 'symbol)))

(defun fingers-move-to-previous-word-occurrence ()
  (interactive)
  (search-backward (thing-at-point 'word)))

(defun fingers-move-to-previous-symbol-occurrence ()
  (interactive)
  (search-backward (thing-at-point 'symbol)))

;;
;; Helpers for manipulation
;;
(defun fingers-open-line-below ()
  (interactive)
  (save-excursion
    (end-of-line)
    (open-line 1)))

(defun fingers-replace-char ()
  (interactive)
  (let ((char-to-insert (read-char "Replace with: ")))
    (delete-char 1)
    (insert char-to-insert)
    (backward-char 1)))

(defun fingers-kill-current-region ()
  (kill-region (point) (mark)))

(defun fingers-duplicate-line ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (set-mark (point))
    (end-of-line)
    (kill-ring-save (point) (mark))
    (open-line 1)
    (forward-char 1)
    (yank)))

;;
;; Main command mode
;;

(define-keys fingers-mode-map
  `(
    ;; left hand -- manipulation
    ;;
    ;; q d r w b
    ;; a s h t g
    ;; z x m c v

    ;; top row
    (d . fingers-duplicate-line)
    (r . anzu-query-replace)
    (R . anzu-query-replace-regexp)
    (w . join-line)
    (b . open-line)

    ;; home row
    (a . fingers-enclose-in-pair)
    (s . fingers-remove-enclosing-pair)
    (h . yank)
    (H . helm-show-kill-ring)
    (t . fingers-kill)
    (g . fingers-replace-char)

    ;; bottom row
    (x . ,fingers-mode-x-map)
    (m . kmacro-start-macro)
    (M . kmacro-end-macro)
    (c . ,fingers-mode-c-map)

    ;; right hand -- navigation
    ;;
    ;; j f u p ; [
    ;; y n e o i '
    ;; k l , . /

    ;; top row
    (j . helm-apropos)
    (fn . point-to-register)
    (ff . jump-to-register)
    (ue . isearch-forward)
    (uu . isearch-repeat-forward)
    (uh . fingers-move-to-next-word-occurrence)
    (ur . fingers-move-to-next-symbol-occurrence)
    (po . isearch-backward)
    (pp . isearch-repeat-backward)
    (ph . fingers-move-to-previous-word-occurrence)
    (pr . fingers-move-to-previous-symbol-occurrence)

    ;; home row
    (y . beginning-of-line)
    (Y . beginning-of-buffer)
    (n . left-char)
    (N . backward-word)
    (e . next-line)
    (E . scroll-up-command)
    (o . previous-line)
    (O . scroll-down-command)
    (i . right-char)
    (I . forward-word)
    (,(intern "'") . end-of-line)
    (,(intern "\"") . end-of-buffer)

    ;; bottom row
    (k . ag-project)
    (K . ag)
    (l . ace-jump-mode)
    (L . ace-jump-char-mode)
    (,(intern ".") . c/helm-jump)
    (> . helm-occur)
    (/ . undo)

    (SPC . fingers-mark)
    ))

;;
;; Selection keys
;;
;; t: whole line
;; w: point until end of line
;; h: word
;; r: symbol
;; s: inside pair
;; a: with pair
;; q: with pair and surrounding whitespace
;; pairs: (), {}, [], <>, '', ""

(defun fingers-mark ()
  (interactive)
  (let ((next-key (read-key "Mark: ")))
    (cond ((= next-key ?t) (fingers-mark-whole-line))
          ((= next-key ?w) (fingers-mark-until-end-of-line))
          ((= next-key ?h) (fingers-mark-word))
          ((= next-key ?r) (fingers-mark-symbol))
          ((= next-key ?s) (fingers-mark-inside-pair))
          ((= next-key ?a) (fingers-mark-with-pair))
          ((= next-key ?q) (fingers-mark-with-pair-and-whitespace))
          (t (set-mark (point))
             (fingers-pass-events (string next-key))))))

(defun fingers-kill ()
  (interactive)
  (if (region-active-p) (fingers-kill-current-region)
    (let ((next-key (read-key "Kill: ")))
      (cond ((= next-key ?t) (fingers-kill-whole-line))
            ((= next-key ?w) (fingers-kill-until-end-of-line))
            ((= next-key ?h) (fingers-kill-word))
            ((= next-key ?r) (fingers-kill-symbol))
            ((= next-key ?s) (fingers-kill-inside-pair))
            ((= next-key ?a) (fingers-kill-with-pair))
            ((= next-key ?q) (fingers-kill-with-pair-and-whitespace))
            (t (fingers-pass-events (string next-key)))))))

(defun fingers-kill-with-pair-and-whitespace ()
  (fingers-mark-with-pair-and-whitespace)
  (fingers-kill-current-region))

(defun fingers-kill-inside-pair ()
  (interactive)
  (fingers-mark-inside-pair)
  (fingers-kill-current-region))

(defun fingers-kill-with-pair ()
  (interactive)
  (fingers-mark-with-pair)
  (fingers-kill-current-region))

(defun fingers-kill-with-pair ()
  (interactive)
  (fingers-mark-with-pair-and-whitespace)
  (fingers-kill-current-region))

(defun fingers-mark-with-pair-and-whitespace ()
  (interactive)
  (fingers-dispatch-with-pair 'fingers-mark-with-pair-strings-and-whitespace))

(defun fingers-enclose-in-pair ()
  (interactive)
  (unless (region-active-p) (fingers-mark))
  (fingers-dispatch-with-pair 'fingers-enclose-in-pair-strings))

(defun fingers-enclose-in-pair-strings (start end)
  (let* ((mark-position (mark))
         (point-position (point))
         (start-position (min mark-position point-position))
         (end-position (max mark-position point-position)))
    (goto-char end-position)
    (insert end)
    (goto-char start-position)
    (insert start)
    (goto-char (+ end-position (length end)))))

(defun fingers-remove-enclosing-pair ()
  (interactive)
  (fingers-dispatch-with-pair 'fingers-remove-enclosing-pair-strings))

(defun fingers-remove-enclosing-pair-strings (start end)
  (fingers-mark-inside-pair-strings start end)
  (let ((start-position (mark)))
    (delete-char (length end))
    (goto-char start-position)
    (delete-char (- (length start)))))

(defun fingers-mark-with-pair-strings-and-whitespace (start end)
  (interactive)
  (fingers-move-point-to-pair-starting-string start end)
  (let ((starting-position (point)))
    (skip-chars-backward " \t")
    (set-mark (point))
    (goto-char starting-position))
  (fingers-move-point-to-pair-ending-string start end)
  (forward-char 1)
  (skip-chars-forward " \t"))

(defun fingers-mark-with-pair ()
  (interactive)
  (fingers-dispatch-with-pair 'fingers-mark-with-pair-strings))

(defun fingers-mark-with-pair-strings (start end)
  (interactive)
  (fingers-move-point-to-pair-starting-string start end)
  (set-mark (point))
  (fingers-move-point-to-pair-ending-string start end)
  (forward-char 1))

(defun fingers-dispatch-with-pair (target &optional default)
  (let ((next-key (read-key "Pair start character: ")))
    (cond ((= next-key ?\() (funcall target "(" ")"))
          ((= next-key ?\{) (funcall target "{" "}"))
          ((= next-key ?\[) (funcall target "[" "]"))
          ((= next-key ?\<) (funcall target "<" ">"))
          ((= next-key ?\') (funcall target "'" "'"))
          ((= next-key ?\") (funcall target "\"" "\""))
          (t (when default (funcall default))
             (fingers-pass-events (string next-key))))))

(defun fingers-mark-inside-pair ()
  (interactive)
  (fingers-dispatch-with-pair 'fingers-mark-inside-pair-strings
                              (lambda () (set-mark (point)))))

(defun fingers-move-point-to-balanced-start (start end)
  (fingers-move-point-to-balanced t start end))

(defun fingers-move-point-to-balanced-end (start end)
  (fingers-move-point-to-balanced nil start end))

(defun fingers-move-point-to-balanced (look-for-start start end)
  (let ((counter 1))
    (while (> counter 0)
      (if look-for-start (backward-char 1) (forward-char 1))
      (cond ((looking-at (regexp-quote (if look-for-start end start))) (setq counter (1+ counter)))
            ((looking-at (regexp-quote (if look-for-start start end))) (setq counter (1- counter)))))))

(defun fingers-move-point-to-pair-start-simple (pair)
  (message "looking for simple [%s]" pair)
  (backward-char 1)
  (while (not (looking-at (regexp-quote pair)))
    (backward-char 1)))

(defun fingers-move-point-to-pair-end-simple (pair)
  (forward-char 1)
  (while (not (looking-at (regexp-quote pair)))
    (forward-char 1)))

(defun fingers-move-point-to-pair-starting-string (start end)
  (if (string= start end)
      (fingers-move-point-to-pair-start-simple start)
    (fingers-move-point-to-balanced-start start end)))

(defun fingers-move-point-to-pair-ending-string (start end)
  (if (string= start end)
      (fingers-move-point-to-pair-end-simple start)
    (fingers-move-point-to-balanced-end start end)))

(defun fingers-mark-inside-pair-strings (start end)
  (interactive)
  (fingers-move-point-to-pair-starting-string start end)
  (forward-char 1)
  (set-mark (point))
  (backward-char 1)
  (fingers-move-point-to-pair-ending-string start end))

(defun fingers-kill-symbol ()
  (interactive)
  (fingers-mark-symbol)
  (fingers-kill-current-region))

(defun fingers-looking-at-symbol ()
  (interactive)
  (looking-at "\\_<"))

(defun fingers-beginning-of-symbol ()
  (interactive)
  (while (not (fingers-looking-at-symbol))
    (left-char 1)))

(defun fingers-mark-symbol ()
  (interactive)
  (unless (fingers-looking-at-symbol) (fingers-beginning-of-symbol))
  (set-mark (point))
  (forward-symbol 1))

(defun fingers-kill-word ()
  (interactive)
  (fingers-mark-word)
  (fingers-kill-current-region))

(defun fingers-looking-at-word ()
  (interactive)
  (looking-at "\\<"))

(defun fingers-beginning-of-word ()
  (interactive)
  (backward-word))

(defun fingers-mark-word ()
  (unless (fingers-looking-at-word) (fingers-beginning-of-word))
  (set-mark (point))
  (forward-word))

(defun fingers-kill-until-end-of-line ()
  (interactive)
  (fingers-mark-until-end-of-line)
  (fingers-kill-current-region))

(defun fingers-mark-until-end-of-line ()
  (interactive)
  (set-mark (point))
  (end-of-line))

(defun fingers-kill-whole-line ()
  (interactive)
  (fingers-mark-whole-line)
  (fingers-kill-current-region)
  (delete-char 1))

(defun fingers-mark-whole-line ()
  (interactive)
  (beginning-of-line)
  (set-mark (point))
  (end-of-line))

;;
;; x-map
;;
(define-keys fingers-mode-x-map
  `(
    (c . save-buffers-kill-terminal)
    (e . eval-last-sexp)
    (f . helm-find-files)
    (k . kill-buffer)
    (o . other-window)
    (s . ,(fingers-pass-events-command "C-x C-s"))
    (x . helm-M-x)
    (0 . delete-window)
    (1 . delete-other-windows)
    (2 . split-window-below)
    (3 . split-window-right)
    ))

;;
;; c-map
;;
(define-keys fingers-mode-c-map
  `(
    (b . ,(fingers-pass-events-command "C-c C-b"))
    (c . ,(fingers-pass-events-command "C-c C-c"))
    (,(intern "'") . ,(fingers-pass-events-command "C-c C-'"))
    ))

;; chords

(key-chord-define-global "oe" 'fingers-mode-toggle-globally)

;;
;; Mode management
;;
(defvar fingers-mode-active nil)
(defvar fingers-mode-excluded-major-modes '(minibuffer-inactive-mode))

(defun fingers-mode-activate ()
  (interactive)
  (fingers-mode 1))

(defun fingers-mode-deactivate ()
  (interactive)
  (fingers-mode -1))

(defun fingers-mode-maybe-activate ()
  (interactive)
  (let ((should-activate (not (member major-mode fingers-mode-excluded-major-modes))))
    (when should-activate
      (fingers-mode-activate))))

(defun fingers-mode-deactivate-globally ()
  (interactive)
  (setq fingers-mode-active nil)
  (remove-hook 'after-change-major-mode-hook 'fingers-mode-maybe-activate)
  (mapc (lambda (buffer) (with-current-buffer buffer (fingers-mode-deactivate)))
        (buffer-list)))

(defun fingers-mode-activate-globally ()
  (interactive)
  (setq fingers-mode-active t)
  (add-hook 'after-change-major-mode-hook 'fingers-mode-maybe-activate)
  (mapc (lambda (buffer) (with-current-buffer buffer (fingers-mode-maybe-activate)))
        (buffer-list)))

(defun fingers-mode-toggle-globally ()
  (interactive)
  (if fingers-mode-active
      (fingers-mode-deactivate-globally)
    (fingers-mode-activate-globally)))

(define-minor-mode fingers-mode
  "Minor mode "
  nil " fingers" fingers-mode-map)

(provide 'fingers)
