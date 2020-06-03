(setq load-path (remove "/usr/share/emacs/26.3/lisp/org" load-path)) 
(add-to-list 'load-path "~/.emacs.d/elpa/org-9.3.6")

(setq vc-follow-symlinks nil)

(if (version< emacs-version "27.0")
    (load-file (expand-file-name "early-init.el" user-emacs-directory))
)


(message (format "%s" load-path))
(setq load-prefer-newer t)
(setq org-src-tab-acts-natively t)
(require 'org)
(org-babel-load-file
(expand-file-name "settings.org"
                  user-emacs-directory))

