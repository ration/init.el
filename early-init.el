(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(show-paren-mode 1)

(if (find-font (font-spec :name "Fira Code"))
    (set-face-attribute 'default nil
                    :family "Fira Code"
                    :height 100
                    :weight 'normal
                    :width 'normal)
)

(setq inhibit-splash-screen t)

