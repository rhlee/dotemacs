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

(add-to-list 'load-path "~/.emacs.d/")

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

(if (eq system-type 'cygwin)
  ()
  (progn
    (require 'dirtree)
    (require 'nav)
    (require 'sml-modeline)
    (require 'fill-column-indicator)

    (defun copy-to-x-clipboard ()
      (interactive)
      (with-current-buffer (current-buffer)
      (call-process-region (region-beginning) (region-end) "xclip" nil 0 nil "-selection" "clipboard")))

    (setq epa-armor t)

    (setq fci-rule-character ?\u2502)
    (setq fci-rule-color "darkblue")
    (setq fci-rule-column 80) ))

(setq rcirc-server-alist `((,(my-config '(:rcirc :server)) :channels ())))
(setq rcirc-default-nick (my-config '(:rcirc :nic)))