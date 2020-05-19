(setq vc-follow-symlinks nil)

(if (version< emacs-version "27.0")
    (load-file (expand-file-name "early-init.el" user-emacs-directory))
)

;; Load the rest from org file
(require 'org)
(setq org-src-tab-acts-natively t)
(org-babel-load-file
 (expand-file-name "settings.org"
                   user-emacs-directory))

