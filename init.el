;;
;; Allow override of some variables depending on local system
;; 

(if (file-exists-p "~/.local.el")
    (load-file "~/.local.el")
  (progn
    (setq git-home "c:/Program Files/Git/")
    (setq dropbox-home "c:/Users/Tatu Lahtela/Dropbox/home/")	  
    ))



;; Settings related to look

(setq font-lock-use-default-fonts nil)
(setq font-lock-use-default-colors nil)
(setq default-frame-alist
      '((foreground-color . "Wheat")
        (cursor-color . "Orchid")
        (background-color . "DarkSlateGray")))
(set-face-background 'mode-line "Wheat")
(set-face-foreground 'mode-line "DarkSlateGray")
(set-face-background 'mode-line"LightSlateGray")
(set-face-foreground 'mode-line "Wheat")

(set-background-color "DarkSlateGray")
(set-foreground-color  "Wheat")

(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(show-paren-mode 1)

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

;; Windows related configurations
(if (string-equal system-type "windows-nt")
    (progn
      (setq find-program (concat git-home "/usr/bin/find.exe"))
      (setq grep-program (concat git-home "/bin/grep.exe"))
      (setq ispell-program-name "C:/Tatu/Apps/hunspell/bin/hunspell.exe")
      ))

;; Various package list
(use-package xml+)
(require 'find-lisp)


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
    :bind* (:map helm-map 
                 ([tab] . helm-execute-if-single-persistent-action)
                 ("C-i" . helm-execute-persistent-action)
                 ))

(use-package helm-files
  :ensure f
  :bind (:map helm-find-files-map
              ("<C-backspace>" . helm-find-files-up-one-level)
              ("<C-backspace>" . helm-find-files-up-one-level)
              )
  )


(use-package helm-swoop
  :config
  (setq helm-swoop-pre-input-function (lambda () "")))

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-c f") 'helm-recentf)
(global-set-key (kbd "C-s") 'helm-swoop)
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
(run-at-time nil (* 5 60) 'recentf-save-list)


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
  


(global-set-key (kbd "M-k") 'kill-line-without-copy)
(global-set-key (kbd "M-<up>") 'other-window)
(defun cycle-backwards ()
  (interactive)
  (other-window -1))
(global-set-key (kbd "M-<down>") 'cycle-backwards)
(global-set-key (kbd "M-<backspace>") 'backward-kill-word-without-copy)


;;
;; Org mode
;; 

(use-package org-super-agenda)
(use-package org-clock-today)

(setq org-agenda-directory (concat dropbox-home "/Documents/Orgzly/"))
;;(setq org-agenda-files
;;      (find-lisp-find-files org-agenda-directory "\.org$"))
(setq org-agenda-files
   (list (concat dropbox-home "/Documents/Orgzly/todo.org")))

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


