;;; vanilla.el --- Alternate init without elpaca  -*- lexical-binding: t; no-byte-compile: t; eval: (outline-minor-mode 1); eval: (outline-hide-sublevels 1); -*-
;;; Commentary:
;;; Same config as init.el but using built-in package.el + use-package with MELPA.
;;; Requires Emacs 29+ (built-in use-package, package-vc for :vc installs).

(setopt use-short-answers t)

;;; Code:

;;; Package Bootstrap

(require 'package)
(add-to-list 'package-archives '("gnu"    . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa"  . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)
(package-initialize)

;; Refresh package list on first run if needed
(unless package-archive-contents
  (package-refresh-contents))

;; use-package is built-in from Emacs 29; ensure :ensure works
(require 'use-package)
(setq use-package-always-ensure t)


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
(global-set-key (kbd "C-c j e") 'next-error)
(global-set-key (kbd "C-c b") 'bufler)
(global-set-key (kbd "C-x b") 'switch-to-buffer)
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "C-ö") 'avy-goto-char-2)

(defvar my-other-map (make-sparse-keymap)
  "A keymap for my frequent other commands.")
(global-set-key (kbd "C-c o") my-other-map)


;;; Outline mode

(use-package emacs
  :ensure nil
  :config
  (keymap-set emacs-lisp-mode-map "TAB" #'outline-cycle)
  :hook (emacs-lisp-mode . outline-minor-mode))


;;; Theme and fonts

(use-package catppuccin-theme
  :init (setq catppuccin-flavor 'mocha)
  :config (load-theme 'catppuccin t))

(use-package nerd-icons)

(use-package nerd-icons-completion
  :after (marginalia nerd-icons)
  :hook (marginalia-mode . nerd-icons-completion-mode))

(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))

(set-face-attribute 'default nil
                    :family "FiraMono Nerd Font"
                    :weight 'regular
                    :height 110)

;; (add-to-list 'default-frame-alist '(font . "FiraMono Nerd Font-17"))
(add-to-list 'default-frame-alist '(font . "IBM Plex Sans-17"))


;;; Rainbow delimiters

(use-package rainbow-delimiters
  :hook
  (emacs-lisp-mode . rainbow-delimiters-mode)
  (python-mode . rainbow-delimiters-mode))


;;; Indentation guides

(use-package outline-indent :defer t)

(use-package indent-bars
  :commands indent-bars-mode
  :hook ((yaml-mode . indent-bars-mode)
         (yaml-ts-mode . indent-bars-mode)
         (python-mode . indent-bars-mode)
         (python-ts-mode . indent-bars-mode))
  :custom
  (indent-bars-prefer-character t))


;;; (Ma)git

(use-package transient :defer t)

(use-package magit
  :bind ([f8] . magit-status))


;;; Minibuffer Completion Stack

;; 1. Vertico: Better vertical interface for M-x, etc.
(use-package vertico
  :init
  (vertico-mode)
  (require 'vertico-directory)
  (require 'vertico-multiform)
  (vertico-multiform-mode)
  (keymap-set vertico-map "RET" #'vertico-directory-enter)
  :config
  (setq vertico-multiform-categories
        '((buffer (vertico-sort-function . nil)))))

;; 2. Marginalia: Adds the "Rich Annotations" (docstrings, etc.)
(use-package marginalia
  :init
  (marginalia-mode))

;; 3. Orderless: Better filtering (type "desc buff" to find "describe-buffer")
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;; 4. Corfu: In-buffer completion (the popup while you type Elisp)
(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-quit-at-boundary 'separator)
  :bind (:map corfu-map
              ("<right>" . corfu-quit)
              ("<left>"  . corfu-quit))
  :init
  (global-corfu-mode))

(use-package consult :defer t)


;;; Random packages

(use-package ace-window :defer t)
(use-package toml-mode :mode "\\.toml\\'")


;;; Org Mode

(use-package org
  :ensure nil  ;; use built-in or already installed
  :config
  (setq org-directory "~/Org/Agenda")
  (setq org-default-notes-file (concat org-directory "/notes.org"))
  (setq org-startup-indented nil)
  (setq org-enforce-todo-dependencies t)
  (setq org-agenda-files
        (directory-files-recursively org-directory "\\.org$"))
  (setq org-refile-use-outline-path 'file)
  (setq org-refile-targets '((nil :maxlevel . 9)
                              (org-agenda-files :maxlevel . 9)))
  (setq org-outline-path-complete-in-steps nil))


;;; org-gtasks (from sourcehut)

(use-package deferred :defer t)
(use-package request :defer t)
(use-package request-deferred :defer t)

(use-package org-gtasks
  :ensure nil
  :vc (:url "https://git.sr.ht/~jmasson/org-gtasks" :rev :newest))


;;; org-modern

(use-package org-modern
  :hook (org-mode . org-modern-mode)
  :config
  (setq org-modern-star '("●" "○" "•" "◦")))



;;; Recent Files (Recentf)

(use-package recentf
  :ensure nil
  :hook (after-init . recentf-mode)
  :custom
  (recentf-max-saved-items 1000)
  (recentf-exclude '(".emacs.d/elpa/" ".emacs.d/recentf")))


;;; Multiple cursors

(use-package multiple-cursors :defer t)


;;; The "Jump" Prefix Map (C-c j)

(defvar my-jump-map (make-sparse-keymap)
  "A keymap for my frequent jump commands.")

(global-set-key (kbd "C-c j") my-jump-map)

(define-key my-jump-map (kbd "r") #'recentf)
(define-key my-jump-map (kbd "f") #'recentf)
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
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))


;;; Project Management (Project.el + Consult)

(defvar my-project-map (make-sparse-keymap)
  "Keymap for project-related commands.")

(global-set-key (kbd "C-c p") my-project-map)

(define-key my-project-map (kbd "p") #'project-switch-project)
(define-key my-project-map (kbd "f") #'project-find-file)
(define-key my-project-map (kbd "s") #'consult-ripgrep)
(define-key my-project-map (kbd "b") #'project-switch-to-buffer)
(define-key my-project-map (kbd "d") #'project-dired)
(define-key my-project-map (kbd "e") #'project-eshell)
(define-key my-project-map (kbd "k") #'project-kill-buffers)


;;; Terraform

(use-package terraform-mode
  :hook (terraform-mode . lsp))

(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration
               '(terraform-mode . "terraform"))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("terraform-ls" "serve"))
                    :major-modes '(terraform-mode)
                    :server-id 'terraform-ls)))


;;; Helpful

(use-package helpful :defer t)
(global-set-key (kbd "C-h f") #'helpful-callable)
(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)
(global-set-key (kbd "C-h x") #'helpful-command)


;;; Which-key

(use-package which-key
  :ensure nil  ;; built-in from Emacs 30
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.5))


;;; yasnippet

(use-package yasnippet :defer t)
(use-package yasnippet-snippets :defer t)


;;; RG and other file finding stuff

(use-package rg
  :defer t
  :init
  (setq xref-search-program 'ripgrep))
(use-package zoxide :defer t)


;;; Window layouts

(winner-mode 1)


;;; No littering

(use-package no-littering
  :init (setq auto-save-file-name-transforms
              `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (require 'recentf)
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory)
  (no-littering-theme-backups))


;;; QML

(use-package qml-mode
  :mode "\\.qml\\'"
  :config
  (setq qml-basic-offset 4))


;;; AI

(use-package track-changes)

;; (use-package copilot
;;   :ensure nil
;;   :hook (prog-mode . copilot-mode))
;; 
;; (use-package polymode :defer t)
;; 
;; (use-package copilot-chat
;;   :vc (:url "https://github.com/chep/copilot-chat.el" :branch "master")
;;   :config
;;   (setq copilot-chat-default-model "gpt-5.2-codex")
;;   (defvar my-copilot-map (make-sparse-keymap))
;;   (global-set-key (kbd "C-c a") my-copilot-map)
;;   (define-key my-copilot-map (kbd "i") #'copilot-chat-ask-and-insert))


;;; vterm

(use-package vterm
  :bind (:map my-other-map ("t" . vterm))
  :config
  (add-to-list 'vterm-eval-cmds '("update-pwd" (lambda (path) (setq default-directory path)))))


;;; My capture org template

(use-package capture-org-template
  :ensure nil
  :vc (:url "https://github.com/ration/capture-org-template.el" :rev :newest)
  :after org
  :config
  (setq org-capture-templates
        (capture-org-template "~/Org/capture.org")))


;;; Swiper

(use-package swiper
  :bind ("C-c s" . swiper))


;;; EMAIL

(use-package mu4e
  :ensure nil
  :defer t
  :load-path "/usr/share/emacs/site-lisp/elpa-src/mu4e-1.10.8/"
  :config
  (setq mail-user-agent 'mu4e-user-agent)
  (setq mu4e-maildir "/home/lahtela/Mail/lahtela"
        mu4e-get-mail-command "mbsync -a"
        mu4e-update-interval 300
        mu4e-attachment-dir  "~/Downloads"
        mu4e-sent-messages-behavior 'sent)
  (setq message-sendmail-f-is-evil t)
  (setq mu4e-drafts-folder "/Drafts")
  (setq mu4e-sent-folder   "/Sent")
  (setq mu4e-trash-folder  "/Trash")
  (setq mu4e-user-mail-address-list '("lahtela@iki.fi"))
  (setq mu4e-maildir-shortcuts
        '(("/INBOX" . ?i)))
  (setq user-mail-address "lahtela@iki.fi"
        user-full-name    "Tatu Lahtela")
  (setq mu4e-change-filenames-when-moving t)
  (setq mu4e-update-interval 300)
  (setq sendmail-program (executable-find "msmtp")
        send-mail-function 'smtpmail-send-it
        message-sendmail-f-is-evil t
        message-sendmail-extra-arguments '("--read-envelope-from")
        message-send-mail-function 'message-send-mail-with-sendmail))

(use-package mu4easy
  :bind ("C-c u" . mu4e)
  :config (mu4easy-mode)
  :custom
  (mu4easy-contexts '((mu4easy-context
                       :c-name  "lahtela@iki.fi"
                       :maildir "lahtela@iki.fi"
                       :mail    "lahtela@iki.fi"
                       :smtp    "mail.kapsi.fi"
                       :sent-action delete))))

(setq mu4e-get-mail-command "mbsync -a")


;;; Swagger

(use-package swagg :defer t)


;;; yaml

(use-package yaml-mode :mode "\\.ya?ml\\'")


;;; Casual Suite

(use-package casual-suite
  :bind (:map org-agenda-mode-map
         ("C-o" . casual-agenda-tmenu)
         :map bookmark-bmenu-mode-map
         ("C-o" . casual-bookmarks-tmenu)))


;;; Load all other lisps

(load-file (concat user-emacs-directory "local.el"))
(load-file (concat user-emacs-directory "functions.el"))
(load-file (concat user-emacs-directory "my-magit.el"))


;;; Agenda

(setq inhibit-startup-screen t)
(add-hook 'after-init-hook (lambda ()
                             (org-agenda nil "a")
                             (delete-other-windows)
                             (switch-to-buffer "*Org Agenda*")) 100)

;;; vanilla.el ends here
