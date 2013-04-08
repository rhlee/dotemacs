(load-file (concat
  (file-name-directory (file-chase-links load-file-name))
  "user-config.el"))

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

(setq-default indent-tabs-mode nil)
(defun newline-keep-indent ()
  (interactive)
  (let ((i (min (current-indentation) (current-column))))
    (newline)
    (indent-to i)))
(global-set-key "\r" 'newline-keep-indent)

(setq epa-armor t)

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
          (start-process "play" "*scratch*" "play" "~/Dropbox/rcirc/me.wav")
          (if (not quiet-mode-flag)
            (start-process
              "play" "*scratch*" "play" "~/Dropbox/rcirc/msg.wav"))))))
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

(windmove-default-keybindings)
(define-key input-decode-map "\e[1;2A" [S-up])
