;;; vanilla-python.el --- Python config without elpaca  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Same as python.el but using built-in package.el + use-package.

;;; Python Development

(use-package python
  :ensure nil
;;   :hook (python-mode . lsp-deferred) ;; If using lsp
  :config
  ;; Remove guess indent messages
  (setq python-indent-guess-indent-offset-verbose nil))

;; 1. Virtual Environment Management
(use-package project
  :ensure nil
  :config
  ;; Tell project.el to recognize .venv as a project marker
  (setq project-vc-extra-root-markers '(".git")))

;; Updated Python venv hook using project.el
(defun my/python-auto-venv-workon ()
  "Activate .venv using project.el's root discovery."
  (interactive)
  (vc-refresh-state)
  (message "working on %s" (vc-root-dir))
  (when-let* ((proj (vc-root-dir)) ;; Note vc-root-dir might not be always correct, but for me it is
              (venv-path (expand-file-name ".venv" proj)))
    (message "activating? %s" venv-path)
    (when (file-directory-p venv-path)
      (message "Activating %s" venv-path)
      (pyvenv-activate venv-path))))

(defun my/python-venv-then-lsp ()
  "Activate .venv first, then start LSP."
  (my/python-auto-venv-workon)
  (lsp-deferred))

(add-hook 'python-mode-hook #'my/python-venv-then-lsp)


(defun my-project-find-file ()
  "Search for files strictly from the project root using Consult."
  (interactive)
  (if-let ((pr (project-current t)))
      (let ((default-directory (project-root pr)))
        (consult-find (project-root pr)))
    (call-interactively #'find-file)))

;;; C-c p map

(defvar my-project-map (make-sparse-keymap)
  "Keymap for project-related commands.")

(global-set-key (kbd "C-c p") my-project-map)

(define-key my-project-map (kbd "p") #'project-switch-project)
(define-key my-project-map (kbd "f") #'my-project-find-file)
(define-key my-project-map (kbd "s") #'project-find-regexp)
(define-key my-project-map (kbd "b") #'project-switch-to-buffer)
(define-key my-project-map (kbd "d") #'project-dired)
(define-key my-project-map (kbd "e") #'project-eshell)
(define-key my-project-map (kbd "k") #'project-kill-buffers)


;; 2. Automatic .venv detection

(use-package pyvenv
  :config
  (setq pyvenv-mode-line-indicator '(pyvenv-virtual-env-name ("[venv:" v " ] ")))
  (pyvenv-mode 1))


;; 3. LSP Support (Pyright & Ruff)

(defun my-python-import-under-cursor ()
  "Import symbol under cursor"
  (interactive)
  (let ((name (thing-at-point 'symbol t)))
    (when name
      (python-add-import name))))

(use-package lsp-mode
  :custom
  (lsp-completion-provider :none)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :bind
  (:map lsp-command-map
        ("e" . flycheck-next-error)
        ("i" . my-python-import-under-cursor))
  :hook
  (lsp-completion-mode . my/lsp-mode-setup-completion)
  :config
  (defun my/lsp-mode-setup-completion ()
    "Set up completion using the correct Cape function."
    (require 'cape)
    (setq-local completion-at-point-functions
                (list (cape-capf-super
                       #'lsp-completion-at-point
                       #'cape-file
                       #'cape-dabbrev)))))

;; 4. Specific Ruff Integration (Formatting/Linting)
(use-package lsp-pyright
  :after lsp-mode)

(use-package consult-lsp :after lsp-mode)

(use-package ruff-format
  :hook (python-mode . ruff-format-on-save-mode))

(use-package lsp-ui
  :commands lsp-ui-mode
  :custom
  (lsp-ui-doc-enable nil)
  (lsp-ui-peek-enable t)
  (lsp-ui-sideline-enable nil))

(use-package flycheck
  :init (global-flycheck-mode)
  :custom
  (flycheck-display-errors-delay 0.5)
  (flycheck-check-syntax-automatically '(save mode-enabled))
  :config
  (setq-default flycheck-disabled-checkers '(python-pylint python-flake8)))


(setq lsp-disabled-clients '(python-pyls))
(setq lsp-ruff-lsp-hover nil)

;;; C-c j map

(define-key my-jump-map (kbd "d") #'lsp-find-definition)
(define-key my-jump-map (kbd "u") #'lsp-find-references)

;;; vanilla-python.el ends here
