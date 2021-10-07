(setq user-init-file (or load-file-name (buffer-file-name)))
(setq user-emacs-directory (file-name-directory user-init-file))

;; Dont prompt about vc symlinks, just goto
(setq vc-follow-symlinks nil)

(load-file (expand-file-name "early-init.el" user-emacs-directory))

;;
;; Straight.el
;;

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(straight-use-package 'el-patch)
(straight-use-package 'org)

(setq straight-use-package-by-default t)

(message (format "%s" load-path))
(setq load-prefer-newer t)


;;
;; Load other packages from settings.org
;;
;;;; Some settings before even entering the actual settings file
;;;; Native indentation of source blocks without indentation
(setq org-edit-src-content-indentation 0)
(setq org-src-tab-acts-natively t)
(require 'org)
;; don't use org-babel-load-file directly because symlinks haywire the org-file-newer-than-p
;; detection
(defun my-org-babel-load-file (file &optional compile)
  "Load Emacs Lisp source code blocks in the Org FILE.
This function exports the source code using `org-babel-tangle'
and then loads the resulting file using `load-file'.  With
optional prefix argument COMPILE, the tangled Emacs Lisp file is
byte-compiled before it is loaded."
  (interactive "fFile to load: \nP")
  (let* ((tangled-file (concat (file-name-sans-extension file) ".el")))
    (org-babel-tangle-file file tangled-file "emacs-lisp")
    (if compile
	(progn
	  (byte-compile-file tangled-file 'load)
	  (message "Compiled and loaded %s" tangled-file))
      (load-file tangled-file)
      (message "Loaded %s" tangled-file))))

(my-org-babel-load-file
 (expand-file-name "settings.org"
                   user-emacs-directory))

