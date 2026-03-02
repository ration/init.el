;;; Hide 
<<<<<<< HEAD
=======
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
>>>>>>> 8ae22ac (gitorizer: auto-commit 2026-03-02T08:15 [early-init.el])

(push '(tool-bar-lines . 0) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)


;; Prevent built-in package.el from loading
(setq package-enable-at-startup nil
      package-archives nil)


(setq frame-inhibit-implied-resize nil) 
(setq frame-resize-pixelwise t)
(setq frame-inhibit-implied-resize t)
(setq redisplay-skip-fontification-on-input t)
(setq fast-but-imprecise-formatting t)
(setq x-gtk-use-system-tooltips nil)

(setq gc-cons-threshold 100000000) ;; 100MB


(setq safe-local-variable-directories
      (mapcar (lambda (dir) (file-name-as-directory (file-truename dir)))
              '("~/.emacs.d/"
                "~/Org/"
                "~/git/Helen/odl/")))

<<<<<<< HEAD
(setq custom-file "~/.emacs.d/custom.el")
=======
(defvar my-trusted-dir-locals
  (list (expand-file-name "~/.emacs.d/")
        (expand-file-name "~/Org/")
        (expand-file-name "~/git/Helen/odl"))
  "List of trusted directories for dir-locals.")

(defun my-allow-dir-locals-p (&rest _args)
  "Return t if the current buffer is within a trusted directory to bypass prompts."
  (let ((current-path (and buffer-file-name (file-truename buffer-file-name))))
    (when current-path
      (seq-some (lambda (dir)
                  ;; file-in-directory-p is more robust than string-prefix-p
                  ;; as it handles slash normalization automatically.
                  (file-in-directory-p current-path (file-truename dir)))
                my-trusted-dir-locals))))

(setq safe-local-variable-directories
      (delete-dups
       (append
        (mapcar (lambda (dir)
                  ;; Must be absolute, expanded, and end with slash
                  (file-name-as-directory
                   (file-truename dir)))
                my-trusted-dir-locals)
        safe-local-variable-directories)))

(advice-add 'hack-local-variables-confirm :before-until #'my-allow-dir-locals-p)
>>>>>>> 8ae22ac (gitorizer: auto-commit 2026-03-02T08:15 [early-init.el])
