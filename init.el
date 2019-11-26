;;
;; Allow override of some variables depending on local system
;; 

(if (file-exists-p "~/.local.el")
    (load-file "~/.local.el")
  (progn
    (setq git-home "c:/Program Files/Git/")
    (setq dropbox-home "c:/Users/Tatu Lahtela/Dropbox/")	  
    ))

;; Set Custom file to another place
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; MELPA with use-package
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

(require 'server)
(unless (server-running-p) (server-start))

(setq compilation-ask-about-save nil)


;;
;; Windows related configurations
;;

;; Load the ssh agent into environment variables if we have the pid file
(defun load-agent-socket-env()
  (interactive)
  (defvar pid_file (concat (getenv "TEMP") "\\" "ssh_agent.pid"))
  (if (file-exists-p pid_file)
      (progn 
      (setenv "SSH_AUTH_SOCK" (save-excursion
                                (with-temp-buffer
                                  (insert-file-contents pid_file)
                                  (goto-char 1)
                                  (re-search-forward "SSH_AUTH_SOCK=\\(.*?\\);")
                                  (match-string 1)
                                  )))
    (setenv "SSH_AGENT_PID" (save-excursion
                              (with-temp-buffer
                                (insert-file-contents pid_file)
                                (goto-char 1)
                                (re-search-forward "SSH_AGENT_PID=\\(.*?\\);")
                                (match-string 1)
                                ))))))





(if (string-equal system-type "windows-nt")
    (progn
      (setq find-program (concat git-home "/usr/bin/find.exe"))
      (setq grep-program (concat git-home "/bin/grep.exe"))
      (setq ispell-program-name "C:/Tatu/Apps/hunspell/bin/hunspell.exe")
      (setq helm-ag-base-command "c:/tatu/bin/ag --vimgrep")
))
;;      (load-agent-socket-env)))

;;
;; Various random package list
;;
(use-package xml+)
(require 'find-lisp)
(use-package restclient)
(use-package magit
  :init
  (setq magit-refresh-status-buffer nil)
)
(use-package yasnippet)
(use-package ssh-agency)
(use-package shackle
  :init
  (require 'shackle)
  (setq helm-display-function 'pop-to-buffer) ; make helm play nice
  (setq helm-swoop-split-window-function 'display-buffer)
  (add-to-list 'shackle-rules
               '("\\`\\*helm.*?\\*\\'" :regexp t :align t :size 0.9))
  (add-to-list 'shackle-rules
               '("\\`\\*Helm.*?\\*\\'" :regexp t :align right :size 0.4))
  :config
  (shackle-mode t))

(use-package company
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-idle-delay 0.2))
(use-package request)
(use-package json-mode)
(use-package powershell)
(use-package org-mru-clock)
(use-package try)
(use-package dockerfile-mode)
(use-package yafolding)
(use-package org-analyzer)
(use-package which-key
  :config (which-key-mode))
(use-package goto-last-change)

(use-package helm-ag
  :init (custom-set-variables
 '(helm-follow-mode-persistent t)))
;;
;; Helm config
;;

(defun double-flash-mode-line ()
  "Flash the modeline"
  (let ((flash-sec (/ 1.0 20)))
    (invert-face 'mode-line)
    (run-with-timer flash-sec nil #'invert-face 'mode-line)
    (run-with-timer (* 2 flash-sec) nil #'invert-face 'mode-line)
    (run-with-timer (* 3 flash-sec) nil #'invert-face 'mode-line)))


(defun helm-execute-if-single-persistent-action (&optional attr split-onewindow)
  "Execute persistent action if the candidate list is less than 2 OR if theres no input and only one non trivial thing to select from"
  (interactive)
  (with-helm-alive-p
    (cond ((and (string= helm-input helm-ff-default-directory) (eq (helm-get-candidate-number) 3))
           (progn
             (helm-next-line)
             (helm-next-line)
             (helm-execute-persistent-action))
           )
          ((> (helm-get-candidate-number) 2) (double-flash-mode-line))
          (t (helm-execute-persistent-action))
          )))

(use-package helm
    :bind (("M-x" . helm-M-x)
	    ("C-x b" . helm-buffers-list)
	    ("C-c f" . helm-recentf)
	    ("M-y" . helm-show-kill-ring)
	    :map helm-map 
                 ([tab] . helm-execute-if-single-persistent-action)
                 ("C-i" . helm-select-action)
                 ))

(use-package helm-files
  :ensure f
  :bind ( ("C-x C-f" . helm-find-files)
	 :map helm-find-files-map
              ("<C-backspace>" . helm-find-files-up-one-level)
              ))

(use-package helm-swoop
  :bind (("C-s" . helm-swoop))
  :config
  (setq helm-swoop-speed-or-color t)
  (setq helm-swoop-pre-input-function (lambda () "")))

(use-package helm-org-rifle)

(helm-mode 1)


;;
;; Blogging
;;

(use-package easy-jekyll
  :config
  (setq easy-jekyll-basedir (concat dropbox-home "git/blog/"))
  (setq easy-jekyll-url "https://lahtela.me")
  (setq markdown-command "pandoc -f markdown -t html -s --mathjax --highlight-style=pygments"))


;; Recentf stores opened files into the recents history
;; To store a list of recent buffers
(recentf-mode 1)
(setq recentf-max-menu-items 100)
(setq recentf-max-saved-items 100)
(defun save-recentf-silently()
  (let ((inhibit-message t))
    (recentf-save-list)))
(run-at-time nil (* 5 60) 'save-recentf-silently)


;; Kill words without copying them
(defun kill-line-without-copy ()
  "Deletes from current position to end of line without putting into the kill-ring."
  (interactive)
  (delete-region (point) (line-end-position))
  )

(defun backward-kill-word-without-copy (arg)
  "Deletes from current backwards word without putting into the kill-ring."
  (interactive "p")
  (delete-region (point) (progn (forward-word (- arg)) (point))))
  



(setq enable-recursive-minibuffers 1)

;;
;; Org mode
;; 
(setq calendar-week-start-day 1)
(use-package org-super-agenda)
(use-package org-clock-today)
(use-package ox-gfm)
(setq org-agenda-directory (concat dropbox-home "/Documents/Orgzly/"))
(setq org-agenda-files
      (find-lisp-find-files org-agenda-directory "\.org$"))

(setq org-capture-templates
   (quote
    (("b" "Bloggging" entry
      (file (concat dropbox-home "/Documents/Orgzly/blog.org"))
      "")
     ("w" "Work Task" entry
      (file "c:/Tatu/doc/todo.org")
      "")
     ("d" "Daily" entry
      (file+olp "c:/Tatu/doc/timetrack.org" "Vincit" "Meetings" "Dailys")
      "**** Daily %T" :clock-in t)
     ("m" "Meeting" entry
      (file+olp "c:/Tatu/doc/timetrack.org" "Vincit" "Meetings")
      "*** $?" :clock-in t)
     
     ("t" "Generic TODO" entry
      (file "c:/tatu/Dropbox/Documents/Orgzly/todo.org")
      "* TODO"))))
;;(setq org-agenda-files
;;   (list (concat dropbox-home "/Documents/Orgzly/todo.org")))

(setq-default org-catch-invisible-edits 'smart)
(setq org-default-notes-file (concat dropbox-home "/Documents/Orgzly/todo.org"))
(setq org-refile-targets '((org-agenda-files . (:maxlevel . 6))))

(add-hook 'auto-save-hook 'org-save-all-org-buffers)


(defun x-org-clock-sum-today ()
  "Visit each file in `org-agenda-files' and return the total time of today's
clocked tasks in minutes."
  (let ((files (org-agenda-files))
        (total 0))
    (org-agenda-prepare-buffers files)
    (dolist (file files)
      (with-current-buffer (find-buffer-visiting file)
        (setq total (+ total (org-clock-sum-today)))))
    total))


(defun x-org-clock-get-clock-string-today ()
  "Form a clock-string, that will be shown in the mode line.
If an effort estimate was defined for the current item, use
01:30/01:50 format (clocked/estimated).
If not, show simply the clocked time like 01:50. All Tasks"
  (let ((clocked-time (x-org-clock-sum-today)))
    (if org-clock-effort
	(let* ((effort-in-minutes (org-duration-to-minutes org-clock-effort))
	       (work-done-str
		(propertize (org-duration-from-minutes clocked-time)
			    'face
			    (if (and org-clock-task-overrun
				     (not org-clock-task-overrun-text))
				'org-mode-line-clock-overrun
			      'org-mode-line-clock)))
	       (effort-str (org-duration-from-minutes effort-in-minutes)))
	  (format (propertize " [%s/%s] (%s)" 'face 'org-mode-line-clock)
		  work-done-str effort-str org-clock-heading))
      (format (propertize " [%s] (%s)" 'face 'org-mode-line-clock)
	      (org-duration-from-minutes clocked-time)
	      org-clock-heading))))


(defun current-clock-time-to-file ()
   (interactive)
   (with-temp-file "~/.emacs.d/.task"
     (if (org-clocking-p)
       (insert (x-org-clock-get-clock-string-today))
       (insert ""))))
(run-with-timer 1 60 'current-clock-time-to-file)
(add-hook 'org-clock-in-hook 'current-clock-time-to-file)
(add-hook 'org-clock-out-hook 'current-clock-time-to-file)



;; Don't taint directories with backup files
(defvar backup-dir (concat "~/emacsbak/" (user-login-name) "/"))
(setq backup-directory-alist (list (cons "." backup-dir)))
(setq auto-save-list-file-prefix
      (concat backup-dir ".auto-saves-"))
(setq auto-save-file-name-transforms
      `((".*" ,backup-dir t)))


;;
;; Various helper functions
;;
(defun sudo-save ()
  "Save file with sudo"
  (interactive)
  (if (not buffer-file-name)
      (write-file (concat "/sudo:root@localhost:" (ido-read-file-name "File:")))
    (write-file (concat "/sudo:root@localhost:" buffer-file-name))))


(defun goto-dir ()
  "Go to a given common directory in dired"
  (interactive)
  (setq dirs '(c:/users/xxlahtft/Downloads/ C:/Tatu/git/ C:/temp/))
  (helm-find-files-1 (completing-read ": " dirs)))

;;
;; Default to bookmark menu
;;

(setq bookmark-save-flag 1)
(setq inhibit-splash-screen t)
(require 'bookmark)
(bookmark-bmenu-list)
(switch-to-buffer "*Bookmark List*")

(use-package cl)
(use-package pcre2el)
(use-package treemacs)

;;
;; plantuml
;;
(setq org-plantuml-jar-path
      (expand-file-name (concat dropbox-home "/home/elisp/java-libs/plantuml.jar")))

(load (expand-file-name (concat dropbox-home "/home/elisp/ob-plantuml.el")))

;; Reload images when babels are executed
(defun shk-fix-inline-images ()
  (when org-inline-image-overlays
    (org-redisplay-inline-images)))

(with-eval-after-load 'org
  (add-hook 'org-babel-after-execute-hook 'shk-fix-inline-images))

;;(org-babel-do-load-languages
;; 'org-babel-load-languages
;; '((emacs-lisp . t)
;;   (plantuml . t)))


(defun my-org-confirm-babel-evaluate (lang body)
  (not (string= lang "plantuml")))  ; don't ask for ditaa
(setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)


(require 'ob-plantuml)




;;

;;
(use-package lsp-mode)
(use-package lsp-ui)
;; (use-package projectile)
;; (use-package lsp-java)
;; 

;; x(use-package lsp-intellij)
;;
;; (use-package company-lsp
;;   :defer t
;;   :init
;;   (push 'company-lsp company-backends)
;;   :config
;;   (add-hook 'java-mode-hook #'lsp))
;;
;;  )
;;

;; (use-package dap-mode
;;   :config
;;   (require 'dap-java)
;;   (dap-mode 1)
;;   (dap-ui-mode t))
;; (use-package helm-projectile)


;;
;; Settings related to look
;; 
(defun theme-slate-grey (&optional frame)
  "Set custom background color."
  (with-selected-frame (or frame (selected-frame))
    (set-background-color "DarkSlateGray")
    (set-face-background 'mode-line "Wheat")
    (set-face-foreground 'mode-line "DarkSlateGray")
    (set-face-background 'mode-line"LightSlateGray")
    (set-face-foreground 'mode-line "Wheat")
    (set-background-color "DarkSlateGray")
    (set-foreground-color  "Wheat")
    ))  

;(add-hook 'after-make-frame-functions 'theme-slate-grey)
;(theme-slate-grey)

;; (use-package dracula-theme)
(use-package color-theme-sanityinc-tomorrow)
(color-theme-sanityinc-tomorrow-eighties)

(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(show-paren-mode 1)
(global-linum-mode 1)



(setq-default frame-title-format '("%f [%m]"))

;;
;; Global key sets
;; 

(global-set-key [f1]  'goto-line)
(global-set-key [f3]  'helm-recentf)
(global-set-key [f4]  'helm-ag)

(global-set-key [f5]  'compile)
(global-set-key [f6]  'next-error)
(global-set-key [f8]  'magit-status)

(global-set-key [f9]  'org-agenda-list)
(global-set-key [f10]  'helm-org-rifle)
(global-set-key [f11]  'org-mru-clock-in)
(global-set-key [f12]  'org-clock-goto)

(global-set-key (kbd "M-k") 'kill-line-without-copy)
(global-set-key (kbd "M-<up>") 'other-window)
(global-set-key (kbd "C-§") 'whitespace-mode)

(defun cycle-backwards ()
  (interactive)
  (other-window -1))

(global-set-key (kbd "M-<down>") 'cycle-backwards)
(global-set-key (kbd "M-<backspace>") 'backward-kill-word-without-copy)
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "<M-S-up>") 'scroll-down-line)
(global-set-key (kbd "<M-S-down>") 'scroll-up-line)

(global-set-key (kbd "M-C-(") (lambda () (interactive) (scroll-down 10)))
(global-set-key (kbd "M-C-)") (lambda () (interactive) (scroll-up 10)))

