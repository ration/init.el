;; TEMP STUFF

(setq debug-on-quit t)

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
