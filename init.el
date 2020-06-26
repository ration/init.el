;; TODO move elpa init here
(require 'package)
(setq package-enable-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package-ensure)
(setq use-package-always-ensure t)

;(setq load-path (remove "/usr/share/emacs/26.3/lisp/org" load-path))
;(add-to-list 'load-path "~/.emacs.d/elpa/org-9.3.6")

;; Dont prompt about vc symlinks, just goto
(setq vc-follow-symlinks nil)

(if (version< emacs-version "27.0")
    (load-file (expand-file-name "early-init.el" user-emacs-directory))
)


(message (format "%s" load-path))
(setq load-prefer-newer t)
;; Some settings before even entering the actual settings file
;; Native indentation of source blocks without indentation
(setq org-edit-src-content-indentation 0)
(setq org-src-tab-acts-natively t)
(require 'org)
(org-babel-load-file
(expand-file-name "settings.org"
                  user-emacs-directory))

(put 'upcase-region 'disabled nil)
