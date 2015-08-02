(setq user-config
  (with-temp-buffer
    (insert-file-contents
      (concat user-emacs-directory "user-config.el"))
    (read (current-buffer))))

(defun get-config (node path)
  (let ((recurse (lambda (f node path)
    (let ((subpath (cdr path)))
      (if (eq path nil)
        node
        (let ((subnode (funcall f f (plist-get node (car path)) subpath)))
          (if (eq subnode nil)
            (error (concat "property not found " (symbol-name (car path))))
            subnode)))))))
    (funcall recurse recurse node path)))

(defun my-config (path) (get-config user-config path))

(require 'mouse)
(xterm-mouse-mode t)
(defun track-mouse (e))
(global-set-key [mouse-4] (lambda () (interactive) (scroll-down 1)))
(global-set-key [mouse-5] (lambda () (interactive) (scroll-up 1)))

(setq backup-directory-alist
  `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
  `((".*" ,temporary-file-directory t)))

(setq epa-armor t)

(require 'rcirc)

(setq rcirc-auto-authenticate-flag t)
(setq rcirc-authenticate-before-join t)
(setq rcirc-server-alist (list (my-config '(:rcirc :server-alist))))
(setq rcirc-authinfo (my-config '(:rcirc :authinfo)))

(defun switch-to-autojoin (process sender response target text)
  (when (string= target 
      (car (plist-get (cdr (car rcirc-server-alist)) :channels)))
    (switch-to-buffer (current-buffer))
    (remove-hook 'rcirc-print-hooks 'switch-to-autojoin))
  nil)

(defun rcirc-notify (process sender response target text)
  (let ((user-nick (buffer-local-value 'rcirc-nick
      (buffer-local-value 'rcirc-server-buffer (current-buffer)))))
    (when (and (string= response "PRIVMSG")(not (string= user-nick sender)))
      (progn
        (if (string-match user-nick text)
          (start-process
            "play"
            "*scratch*"
            "play"
            (concat user-emacs-directory "sounds/me.wav"))
          (if (not quiet-mode-flag)
            (start-process
              "play"
              "*scratch*"
              "play"
              (concat user-emacs-directory "sounds/msg.wav")))))))
  nil)

(defvar quiet-mode-flag nil)

(defun quiet-mode ()
  (interactive)
  (if quiet-mode-flag
    (progn
      (setq quiet-mode-flag nil)
      (message "quiet mode off"))
    (progn 
      (setq quiet-mode-flag t)
      (message "quiet mode on"))))

(add-hook 'rcirc-print-hooks 'switch-to-autojoin)
(add-hook 'rcirc-print-hooks 'rcirc-notify)

(defun copy ()
  (interactive)
  (shell-command-on-region
    (region-beginning) (region-end)
      (if (eq system-type 'cygwin) "putclip" "xsel -ib") nil nil nil t))

(defun paste ()
  (interactive)
  (insert (shell-command-to-string
    (if (eq system-type 'cygwin) "getclip" "xsel -ob"))))

(setq gnus-select-method '(nnimap "gmail"
				  (nnimap-address "imap.gmail.com")
				  (nnimap-server-port 993)
				  (nnimap-stream ssl)))

(show-paren-mode)

(defun freenode (arg)
  (interactive "sAuto-join channel (leave blank for none): ")
  (setq rcirc-server-alist (list
    (append
      (my-config '(:rcirc :freenode))
        (if (string= arg "")
          nil
          (progn
            (add-hook 'rcirc-print-hooks 'switch-to-autojoin)
            (list :channels (list arg)))))))
  (rcirc nil))

(defmacro setf-if (place if-val expr)
  `(let ((val ,place))
    (if (eq val ,if-val)
      (setf ,place ,expr)
      val)))

(setq rcirc-log-flag t)

(setq rcirc-log-filename (make-hash-table))

(defun rcirc-generate-custom-log-filename (process target)
  (let
    ((filename
      (concat
        (replace-regexp-in-string "/" "_"
          (rcirc-generate-log-filename process target))
        (format-time-string ",%Y-%m-%d.%H%M%S"))))
    (setf-if
      (gethash target (setf-if (gethash process rcirc-log-filename) nil
        (make-hash-table :test 'equal)))
      nil
      filename)))

(setq rcirc-log-filename-function 'rcirc-generate-custom-log-filename)

(defun rcirc-set-new-log-file ()
  (if rcirc-target
    (progn
      (rcirc-log-write)
      (remhash
        rcirc-target
        (gethash
          (get-buffer-process rcirc-server-buffer) rcirc-log-filename)))))

(add-hook 'rcirc-mode-hook 'rcirc-set-new-log-file)

(defun rcirc-write-and-close-logs (process sentinel)
  (if (eq (process-status process) 'closed)
    (remhash process rcirc-log-filename))
  (rcirc-log-write))

(add-hook 'rcirc-sentinel-functions 'rcirc-write-and-close-logs)

(add-hook 'kill-emacs-hook 'rcirc-log-write())

(load-file
  (concat
    (file-name-directory load-file-name)
    (file-name-as-directory "timer")
    "timer.el"))

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

(add-hook 'after-change-major-mode-hook
  (lambda ()
    (if (member major-mode simple-indent-mode-autoload-list)
      (simple-indent-mode t))))

(setq simple-indent-mode-autoload-list
  '(emacs-lisp-mode))

(windmove-default-keybindings 'meta)
