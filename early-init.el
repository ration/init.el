;; (setq debug-on-error t)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(show-paren-mode 1)
(setq vc-follow-symlinks t)
(if (find-font (font-spec :name "FiraMono Nerd"))
    (set-face-attribute 'default nil
                    :family "FiraMono Nerd"
                    :height 100
                    :weight 'normal
                    :width 'normal)
)

(setq inhibit-splash-screen t)

;; emacs-ng setting for straight
;; (setq ng-straight-bootstrap-at-startup t)
