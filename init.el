(require 'mouse)
(xterm-mouse-mode t)
(defun track-mouse (e))
(global-set-key [mouse-4] (lambda () (interactive) (scroll-down 1)))
(global-set-key [mouse-5] (lambda () (interactive) (scroll-up 1)))

(setq backup-directory-alist
  `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
  `((".*" ,temporary-file-directory t)))

(show-paren-mode)

(defvar simple-indent-mode-autoload-list nil)

(define-minor-mode simple-indent-mode
  "Simple indentation"
  :lighter " si"
  :keymap
    (let ((keymap (make-sparse-keymap)))
      (dolist (key (number-sequence ?  ?~))
        (define-key keymap (string key) 'self-insert-command))
      (define-key keymap (kbd "RET") 'newline-keep-indent)
      (define-key keymap (kbd "TAB") 'self-insert-command)
      (define-key keymap (kbd "DEL") 'delete-backward-char)
      keymap)
  :after-hook (setq post-self-insert-hook nil))

(defun newline-keep-indent ()
  (interactive)
  (let
    ((line (buffer-substring (line-beginning-position) (line-end-position))))
    (newline)
    (string-match "^[[:blank:]]*" line)
    (insert (match-string 0 line))))

(setq simple-indent-mode-autoload-list
  '(emacs-lisp-mode))

(windmove-default-keybindings 'meta)
