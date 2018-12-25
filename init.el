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

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA
  ;; Stable as desired
  (add-to-list 'package-archives
    (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives
  ;;  (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives
      (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (ranger php-mode js2-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
