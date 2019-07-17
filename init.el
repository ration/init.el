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

;;
;; Windows related configurations
;;

;; Load the ssh agent into environment variables if we have the pid file
(defun load-agent-socket-env()
  (interactive)
  (defvar pid_file (concat (getenv "TEMP") "\\" "ssh_agent.pid"))
  (if (file-exists-p pid_file)
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
                                )))))





(if (string-equal system-type "windows-nt")
    (progn
      (setq find-program (concat git-home "/usr/bin/find.exe"))
      (setq grep-program (concat git-home "/bin/grep.exe"))
      (setq ispell-program-name "C:/Tatu/Apps/hunspell/bin/hunspell.exe")
      (setq helm-ag-base-command "c:/tatu/bin/ag --vimgrep")
      (load-agent-socket-env)))

;;
;; Various random package list
;;
(use-package xml+)
(require 'find-lisp)
(use-package restclient)
(use-package magit)


(use-package company
  :init
  (add-hook 'after-init-hook 'global-company-mode))
(use-package request)
(use-package json-mode)
(use-package powershell)
(use-package try)

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
  (setq markdown-command 
      "pandoc -f markdown -t html -s --mathjax --highlight-style=pygments")

  )


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
  


;; Global keyboard changes
(global-set-key (kbd "M-k") 'kill-line-without-copy)
(global-set-key (kbd "M-<up>") 'other-window)
(defun cycle-backwards ()
  (interactive)
  (other-window -1))
(global-set-key (kbd "M-<down>") 'cycle-backwards)
(global-set-key (kbd "M-<backspace>") 'backward-kill-word-without-copy)
(global-set-key (kbd "M-z") 'zap-up-to-char)

(setq enable-recursive-minibuffers 1)

;;
;; Org mode
;; 

(use-package org-super-agenda)
(use-package org-clock-today)

(setq org-agenda-directory (concat dropbox-home "/Documents/Orgzly/"))
(setq org-agenda-files
      (find-lisp-find-files org-agenda-directory "\.org$"))
;;(setq org-agenda-files
;;   (list (concat dropbox-home "/Documents/Orgzly/todo.org")))

(setq-default org-catch-invisible-edits 'smart)
(setq org-default-notes-file (concat dropbox-home "/Documents/Orgzly/todo.org"))
(setq org-refile-targets '((org-agenda-files . (:maxlevel . 6))))


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


;; Various helper functions
(defun sudo-save ()
  "Save file with sudo"
  (interactive)
  (if (not buffer-file-name)
      (write-file (concat "/sudo:root@localhost:" (ido-read-file-name "File:")))
    (write-file (concat "/sudo:root@localhost:" buffer-file-name))))




;;
;; Default to bookmark menu
;;

(setq bookmark-save-flag 1)
(setq inhibit-splash-screen t)
(require 'bookmark)
(bookmark-bmenu-list)
(switch-to-buffer "*Bookmark List*")


;;
;; Company lsp (TODO, doesn't work)
;;
;;(use-package lsp-mode)
;;(use-package lsp-ui)
;;(use-package lsp-clients)
;;(use-package lsp-intellij)
;;
;;(use-package company-lsp
;;  :init
;;  (push 'company-lsp company-backends)
;;  :config
;;  (add-hook 'java-mode-hook #'lsp-intellij-enable)
;;
;;  )
;;




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

(global-set-key [f1]  'goto-line)
(global-set-key [f9]  'org-agenda-list)
(global-set-key [f12]  'helm-ag)

