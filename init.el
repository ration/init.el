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
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; 2. Install/Enable use-package support
(elpaca elpaca-use-package
  (elpaca-use-package-mode)
  (setq elpaca-use-package-always-ensure t))

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
  :hook (emacs-lisp-mode . rainbow-delimiters-mode))

;;; (Ma)git

;; 1. Force a modern version of Transient
(use-package transient
  :ensure t)

;; 2. Tell Elpaca to wait until Transient is actually finished 
(elpaca-wait)

;; 3. Now install Magit
(use-package magit
  :ensure t
  :bind ([f8] . magit-status))

;;; Load functions.el

(load-file (concat user-emacs-directory "functions.el"))


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


;;; Cape

(use-package cape
  :ensure t
  :init
  ;; Add useful defaults to the completion-at-point-functions
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))


;; smart tab
(defun my/smart-tab ()
  "Context-sensitive tab: cycle outlines, complete, or respect mode-local tab."
  (interactive)
  (cond
   ;; 1. If in Magit, let Magit handle it
   ((derived-mode-p 'magit-mode)
    (call-interactively (key-binding (kbd "TAB"))))
   
   ;; 2. If on an Elisp/Org header, cycle visibility
   ((and (bound-and-true-p outline-minor-mode)
         (save-excursion (beginning-of-line) (looking-at outline-regexp)))
    (outline-cycle))
   
   ;; 3. If there's something to complete, complete it
   ((looking-back "[a-zA-Z0-9-]+" (line-beginning-position))
    (completion-at-point))
   
   ;; 4. Otherwise, just indent
   (t (indent-for-tab-command))))

;; Bind it globally so it works in Python and Elisp
(global-set-key (kbd "<tab>") #'my/smart-tab)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("c3076fdee603e9768817cfe8dbf6253d5b3cf3bf4602cb32fa2f1df62fe70b1c"
     default))
 '(package-selected-packages '(copilot))
 '(package-vc-selected-packages
   '((copilot :url "https://github.com/copilot-emacs/copilot.el" :branch
	      "main")))
 '(safe-local-variable-values
   '((eval outline-hide-sublevels 1) (chore-backend . "azure-devops")))
 '(warning-suppress-types '((use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


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

(use-package terraform-mode :ensure t)

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

;;; RG

(use-package rg :ensure t
  :init
  (setq xref-search-program 'ripgrep))


;;; window layouts
(winner-mode 1)

(use-package window-purpose
  :ensure t
  :init
  (purpose-mode)
  :config
  ;; 1. Define which modes belong to which "Purpose"
  (add-to-list 'purpose-user-mode-purposes '(prog-mode . edit))
  (add-to-list 'purpose-user-mode-purposes '(text-mode . edit))
  (add-to-list 'purpose-user-mode-purposes '(help-mode . doc))
  (add-to-list 'purpose-user-mode-purposes '(helpful-mode . doc))
  (add-to-list 'purpose-user-mode-purposes '(messages-buffer-mode . logs))
  (add-to-list 'purpose-user-mode-purposes '(compilation-mode . logs))

  ;; 2. Update the purpose configuration
  (purpose-compile-user-configuration)

  ;; 3. (Optional) Force specific buffer names to purposes
  (add-to-list 'purpose-user-regexp-purposes '("^\\*Errors\\*$" . logs))
  (add-to-list 'purpose-user-regexp-purposes '("^\\*Messages\\*$" . logs)))

;; No littering

(use-package no-littering
  :ensure t
  :init (setq auto-save-file-name-transforms
                `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))
;;; AI

(use-package track-changes
  :ensure t)
(use-package copilot
  :ensure t
  :vc (:url "https://github.com/copilot-emacs/copilot.el"
            :rev :newest
            :branch "main")
  :after (copilot-install-server)
  :hook (prog-mode . copilot-mode))

(use-package copilot-chat :ensure t)

;;; vterm

(use-package vterm :ensure t
  :init
  (define-key my-other-map (kbd "t") #'vterm)
  )

;; my templates

(use-package capture-org-template
  :ensure t
  :elpaca (capture-org-template :host github :repo "ration/capture-org-template.el")
  :init
  ;; It's often safer to set the variable in :init so it's ready 
  ;; the moment Org-capture is called.
  (setq org-capture-templates (capture-org-template "~/Org/capture.org")))
;;; Swiper

(use-package swiper
  :ensure t
  :config
  (global-set-key (kbd "C-c s") #'swiper))


;;; init.el ends here
