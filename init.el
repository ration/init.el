;;; init.el --- My emacs init file  -*- lexical-binding: t; no-byte-compile: t; eval: (outline-minor-mode 1); eval: (outline-hide-sublevels 1); -*-
;;; Commentary:
;;; Emacs configuration in outline blocks, uses elpaca
;;; Code:

;;; Elpaca Bootstrap

(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load (expand-file-name "elpaca-autoloads.el" repo))))

;; 1. Initialize Elpaca Queue
(add-hook 'after-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; 2. Install/Enable use-package support
(elpaca elpaca-use-package
  (elpaca-use-package-mode)
  (setq elpaca-use-package-always-ensure t))
(setq elpaca-use-package-quiet t)
(elpaca-wait)


;; Now you can safely use (use-package ...) for everything else.
;;; Outline mode

(use-package emacs
  :ensure nil
  :hook (emacs-lisp-mode . outline-minor-mode)
  :config
  (defun my/elisp-tab-dwim ()
    "Cycle outline if on a header, otherwise indent normally."
    (interactive)
    (if (save-excursion
          (beginning-of-line)
          (looking-at outline-regexp))
        (outline-cycle)
      (indent-for-tab-command)))

  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (setq-local outline-regexp ";;;+ ")
              ;; Bind TAB to our smart "Do What I Mean" function
              (local-set-key (kbd "<tab>") #'my/elisp-tab-dwim))))


;;; My global keys

(global-set-key (kbd "C-z") 'undo)
(global-set-key [f8]  'magit-status)
(global-set-key [f12]  'org-capture)

(global-set-key (kbd "C-c j j") 'my-avy-goto-line)
(global-set-key (kbd "C-c j c") 'avy-goto-char-timer)
(global-set-key (kbd "C-c j t") 'treemacs-select-window)
(global-set-key (kbd "C-c j w") 'ace-window)
(global-set-key (kbd "C-c j s") 'scratch)
(global-set-key (kbd "C-c j g") 'goto-line)
(global-set-key (kbd "C-c j n") 'flycheck-next-error)
(global-set-key (kbd "C-c b") 'bufler)
(global-set-key (kbd "C-x b") 'switch-to-buffer)
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "C-ö") 'avy-goto-char-2)

(defvar my-other-map (make-sparse-keymap)
  "A keymap for my frequent other commands.")
(global-set-key (kbd "C-c o") my-other-map)



;;; Theme and fonts

(use-package catppuccin-theme
  :ensure t
  :init (setq catppuccin-flavor 'mocha)
  :config (load-theme 'catppuccin t))

  

;; (use-package darcula-theme
;;   :ensure t
;;   :init
;;   (load-theme 'darcula t))

(use-package nerd-icons
  :ensure t)

(use-package nerd-icons-completion
  :ensure t
  :after (marginalia nerd-icons)
  :hook (marginalia-mode . nerd-icons-completion-mode))

(use-package nerd-icons-dired
  :ensure t
  :hook (dired-mode . nerd-icons-dired-mode))


(set-face-attribute 'default nil 
                    :family "FiraMono Nerd Font" 
                    :weight 'regular 
                    :height 110)

;; Optional: Ensure new frames (windows) use the same font
(add-to-list 'default-frame-alist '(font . "FiraMono Nerd Font-17"))

;;; Rainbow delimetirs

(use-package rainbow-delimiters
  :ensure t
  :hook
  (emacs-lisp-mode . rainbow-delimiters-mode)
  (python-mode . rainbow-delimiters-mode))
;;; Indentation guides

(use-package outline-indent :ensure t)

(use-package indent-bars
  :ensure t
  :commands indent-bars-mode
  :hook ((yaml-mode . indent-bars-mode)
         (yaml-ts-mode . indent-bars-mode)
         (python-mode . indent-bars-mode)
         (python-ts-mode . indent-bars-mode))
  :custom
  (indent-bars-prefer-character t))

;;; (Ma)git

;; 1. Force a modern version of Transient
(use-package transient
  :ensure t)

;; 3. Now install Magit
(use-package magit
  :ensure t
  :bind ([f8] . magit-status))




;;; Minibuffer Completion Stack

;; 1. Vertico: Better vertical interface for M-x, etc.
(use-package vertico
  :ensure t
  :init
  (vertico-mode))

;; 2. Marginalia: Adds the "Rich Annotations" (docstrings, etc.)
(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

;; 3. Orderless: Better filtering (type "desc buff" to find "describe-buffer")
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;; 4. Corfu: In-buffer completion (the popup while you type Elisp)
(use-package corfu
  :ensure t
  :custom
  (corfu-auto t)
  (corfu-quit-at-boundary 'separator)
  :bind (:map corfu-map
              ("<right>" . corfu-quit)
              ("<left>"  . corfu-quit))
  :init
  (global-corfu-mode)
  )

(use-package consult
  :ensure t)


;; Random packages

(use-package ace-window :ensure t)
(use-package toml-mode :ensure t)
;;; Org Mode

(use-package org
  :ensure (org :repo "https://https.git.savannah.gnu.org/git/emacs/org-mode.git" 
               :local-repo "org"
               :branch "main")
  :config
  (setq org-directory "~/Org/Agenda")
  (setq org-default-notes-file (concat org-directory "/notes.org"))
  (setq org-startup-indented nil)
  (setq org-enforce-todo-dependencies t)
  (setq org-agenda-files 
        (directory-files-recursively org-directory "\\.org$")))

;;; org-gtasks

(use-package deferred :ensure t)
(use-package request :ensure t)
(use-package request-deferred :ensure t)

(elpaca
  (org-gtasks
   :repo "https://git.sr.ht/~jmasson/org-gtasks"))
(elpaca-wait)
(use-package org-gtasks)

;;; org-modern

(use-package org-modern :ensure t
  :hook (org-mode . org-modern-mode))

;;; Recent Files (Recentf)
(use-package recentf
  :ensure nil ;; Built-in
  :hook (after-init . recentf-mode)
  :custom
  (recentf-max-saved-items 1000)
  ;; Prevent tracking files inside the elpaca/package folders
  (recentf-exclude '(".emacs.d/elpaca/" ".emacs.d/recentf")))

;;; Multiple cursors

(use-package multiple-cursors :ensure t)

;;; The "Jump" Prefix Map (C-c j)
(defvar my-jump-map (make-sparse-keymap)
  "A keymap for my frequent jump commands.")

;; Bind the map to the C-c j prefix
(global-set-key (kbd "C-c j") my-jump-map)

;;; Add your commands to the map
(define-key my-jump-map (kbd "r") #'recentf) ; The "C-c j r" you asked for
(define-key my-jump-map (kbd "f") #'recentf) ; The "C-c j r" you asked for
(define-key my-jump-map (kbd "j") #'my-avy-goto-line)
(define-key my-jump-map (kbd "c") #'avy-goto-char-timer)
(define-key my-jump-map (kbd "t") #'treemacs-select-window)
(define-key my-jump-map (kbd "w") #'ace-window)
(define-key my-jump-map (kbd "s") #'scratch)
(define-key my-jump-map (kbd "g") #'goto-line)
(define-key my-jump-map (kbd "n") #'flycheck-next-error)
(define-key my-jump-map (kbd "p") #'mc/edit-lines)


;;; Python

(load-file (concat user-emacs-directory "python.el"))

;;; Avy

(use-package avy :ensure t)

;;; Cape

(use-package cape
  :ensure t
  :init
  ;; Add useful defaults to the completion-at-point-functions
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))


;; smart tab
(defun my/smart-tab ()
  "Context-sensitive TAB: delegate to major modes, otherwise complete or indent."
  (interactive)
  (cond
   ;; 1) Magit: let Magit handle TAB
   ((derived-mode-p 'magit-mode)
    (call-interactively
     (or (local-key-binding (kbd "TAB"))
         #'indent-for-tab-command)))

   ;; 2) Org: always delegate to Org's own TAB binding
   ((derived-mode-p 'org-mode)
    (let ((cmd (or (local-key-binding (kbd "TAB"))
                   (local-key-binding (kbd "<tab>")))))
      (when (commandp cmd)
        (call-interactively cmd))))

   ;; 3) Other outline-based modes: cycle headings
   ((and (bound-and-true-p outline-minor-mode)
         (save-excursion
           (beginning-of-line)
           (looking-at-p outline-regexp)))
    (if (fboundp 'outline-cycle)
        (outline-cycle)
      (outline-toggle-children)))

   ;; 4) Completion
   ((looking-back "[[:alnum:]-]+"
                  (line-beginning-position))
    (completion-at-point))

   ;; 5) Fallback
   (t
    (indent-for-tab-command))))


;; Bind it globally so it works in Python and Elisp
(global-set-key (kbd "<tab>") #'my/smart-tab)


;; Project.el

;;; Project Management (Project.el + Consult)


;;; Project Prefix (C-c p)
(defvar my-project-map (make-sparse-keymap)
  "Keymap for project-related commands.")

(global-set-key (kbd "C-c p") my-project-map)

;; Bind project.el commands
(define-key my-project-map (kbd "p") #'project-switch-project)
(define-key my-project-map (kbd "f") #'project-find-file)
;;(define-key my-project-map (kbd "s") #'project-find-regexp)   ;; Search in project
(define-key my-project-map (kbd "s") #'consult-ripgrep)
(define-key my-project-map (kbd "b") #'project-switch-to-buffer)
(define-key my-project-map (kbd "d") #'project-dired)
(define-key my-project-map (kbd "e") #'project-eshell)
(define-key my-project-map (kbd "k") #'project-kill-buffers)


;;; Terraform

(use-package terraform-mode :ensure t
  :hook (terraform-mode . lsp))

(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration
	       '(terraform-mode . "terraform"))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("terraform-ls" "serve"))
                    :major-modes '(terraform-mode)
                    :server-id 'terraform-ls)))

;;; Helpful

(use-package helpful :ensure t)
(global-set-key (kbd "C-h f") #'helpful-callable)

(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)
(global-set-key (kbd "C-h x") #'helpful-command)

;;; Which-key

(use-package which-key
  :ensure nil ;; It's built-in!
  :config
  (which-key-mode)
  ;; Optional: make it pop up faster (default is 1.0 second)
  (setq which-key-idle-delay 0.5))

;;; RG and other file finding stuff

(use-package rg :ensure t
  :init
  (setq xref-search-program 'ripgrep))
(use-package zoxide :ensure t)


;;; window layouts
(winner-mode 1)

;; (use-package window-purpose
;;   :ensure t
;;   :init
;;   (purpose-mode)
;;   :config
;;   (setq purpose-user-mode-purposes
;;       '((prog-mode . edit)
;;         (text-mode . edit)
;;         (magit-status-mode . edit)
;; 	(org-mode  . doc)
;;         (help-mode . doc)
;;         (messages-buffer-mode . logs)
;;         (magit-process-mode . logs)
;;         (compilation-mode . logs)))
;; 
;;   (setq purpose-user-regexp-purposes
;; 	'(("^\\*[hH]elp\\*$" . docs)
;;           ("^\\*[cC]hat\\*$" . docs)
;;           ("^\\*[Ee]rror\\*$" . logs)
;; 	  ("^\\*[Ww]arn\\*$" . logs)
;;           ("^\\*Messages\\*$" . logs)))
;;   (setq purpose-use-default-configuration t)
;;   (purpose-compile-user-configuration))
;; 

;;purpose-user-mode-purposes

(defun my-setup-permanent-layout ()
  "Permanent 2x2 purpose layout:
TL=edit (current buffer)
TR=doc (Emacs manual)
BR=logs (*Messages*)
BL=general (*scratch*)"
  (interactive)
  (require 'window-purpose)
  (purpose-mode 1)
  (purpose-compile-user-configuration)

  (delete-other-windows)

  (let* ((w-edit (selected-window))                 ; top-left
         (w-doc  (split-window w-edit nil 'right))  ; top-right
         (w-logs (split-window w-doc  nil 'below))  ; bottom-right
         (w-gen  (split-window w-edit nil 'below))  ; bottom-left
         (b-scratch  (get-buffer-create "*scratch*"))
         (b-messages (get-buffer-create "*Messages*")))

    ;; 1) Put the exact buffers where we want them (by selecting the window)
    (set-window-buffer w-gen b-scratch)
    (set-window-buffer w-logs b-messages)

    ;; Open the Emacs manual *in the doc window*, without display routing:
    (with-selected-window w-doc
      (info "emacs"))

    ;; 2) Now assign purposes to windows
    (set-window-parameter w-edit 'purpose 'edit)
    (set-window-parameter w-doc  'purpose 'doc)
    (set-window-parameter w-logs 'purpose 'logs)
    (set-window-parameter w-gen  'purpose nil)

    ;; 3) Mark purpose dedication (window-purpose) + Emacs dedication
    (set-window-parameter w-edit 'purpose-dedicated t)
    (set-window-parameter w-doc  'purpose-dedicated t)
    (set-window-parameter w-logs 'purpose-dedicated t)
    (set-window-parameter w-gen  'purpose-dedicated nil)

    ;; (Optional but helps): also pin buffer purposes explicitly
    (purpose-set-buffer-purpose (window-buffer w-doc) 'doc)
    (purpose-set-buffer-purpose b-messages 'logs)

    (select-window w-edit)
    (balance-windows)))


;;; No littering

(use-package no-littering
  :ensure t
  :init (setq auto-save-file-name-transforms
              `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (require 'recentf)
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory)
  (no-littering-theme-backups))

;;; QML

(use-package qml-mode
  :elpaca t
  :mode "\\.qml\\'"
  :config
  ;; This block only runs AFTER you open a .qml file
  (setq qml-basic-offset 4))

;;; AI

(use-package track-changes
  :ensure t)
(use-package copilot
  :ensure t
  :vc (:url "/home/lahtela/git/github/copilot.el"
            :rev :newest
            :branch "main")
  :after (copilot-install-server)
  :hook (prog-mode . copilot-mode)
  )




(use-package copilot-chat
  :ensure t
  :vc (:url "/home/lahtela/git/github/copilot-chat.el"
            :rev :newest
            :branch "master")
  :config
  (defvar my-copilot-map (make-sparse-keymap))
  (global-set-key (kbd "C-c a") my-copilot-map)
  (define-key my-copilot-map (kbd "i") #'copilot-chat-ask-and-insert)
)

;;; vterm

(use-package vterm :ensure t
  :config
  (define-key my-other-map (kbd "t") #'vterm)
  (add-to-list 'vterm-eval-cmds '("update-pwd" (lambda (path) (setq default-directory path)))))

;;; My capture org template

(elpaca
  (capture-org-template
   :repo "ration/capture-org-template.el"))
(elpaca-wait)
(use-package capture-org-template
  :after org
  :config
  (setq org-capture-templates
        (capture-org-template "~/Org/capture.org")))

;;; Swiper

(use-package swiper
  :ensure t
  :config
  (global-set-key (kbd "C-c s") #'swiper))



;;; Swagger

(use-package swagg :ensure t)

;;; yaml

(use-package yaml-mode :ensure t)

;;; git-auto-save

(use-package git-auto-commit-mode :ensure t
  :config
  (setq gac-automatically-push-p t)
  )


;;; Dashboard
(use-package dashboard
  :ensure t
  :after elpaca
  :config (dashboard-setup-startup-hook))

;;; Elpacaa

(elpaca-wait)

;;; Load all other lisps
(elpaca-wait)
(load-file (concat user-emacs-directory "local.el"))
(load-file (concat user-emacs-directory "functions.el"))
(load-file (concat user-emacs-directory "my-magit.el"))
;;; init.el ends here
