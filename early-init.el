;;; Hide 
(setq custom-file "~/.emacs.d/custom.el")

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


(defun my-allow-dir-locals-p (file)
  "Return non-nil if FILE is within a trusted directory."
  (let ((trusted-dir (expand-file-name "~/.emacs.d/")))
    (string-prefix-p trusted-dir (expand-file-name file))))

(advice-add 'save-util-local-variables-check-safe :before-until #'my-allow-dir-locals-p)
