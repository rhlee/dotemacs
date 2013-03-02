(load-file (concat
  (file-name-directory (file-chase-links load-file-name))
  "temp.el"))

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
(setq rcirc-authinfo (list (my-config '(:rcirc :authinfo))))

(defun switch-to-autojoin (process sender response target text)
  (when (string= target 
      (car (plist-get (cdr (my-config '(:rcirc :server-alist))) :channels)))
    (switch-to-buffer (current-buffer))
    (remove-hook 'rcirc-print-hooks 'switch-to-autojoin))
  nil)

(defun rcirc-notify (process sender response target text)
  (when (string= response "PRIVMSG")
    (progn
    (if (string-match
        (buffer-local-value 'rcirc-nick
          (buffer-local-value 'rcirc-server-buffer (current-buffer)))
        text)
      (start-process "ls" "*scratch*" "play" "~/Dropbox/rcirc/me.wav")
      (start-process "ls" "*scratch*" "play" "~/Dropbox/rcirc/msg.wav"))))
  nil)

(add-hook 'rcirc-print-hooks 'switch-to-autojoin)
(add-hook 'rcirc-print-hooks 'rcirc-notify)
