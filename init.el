(if (version< emacs-version "27.0")
    (load-file (expand-file-name "early-init.el" user-emacs-directory))
)

;; Load the rest from org file
(require 'org)
(setq org-src-tab-acts-natively t)
(org-babel-load-file
 (expand-file-name "settings.org"
                   user-emacs-directory))



;;
;; Global key sets
;; 
(global-set-key [f1]  'goto-line)
(global-set-key [f2]  'helm-projects-find-files)
(global-set-key [f3]  'helm-recentf)
(global-set-key [f4]  'helm-ag)

(global-set-key [f5]  'compile)
(global-set-key [f6]  'next-error)
(global-set-key [f8]  'magit-status)

(global-set-key [f9]  'org-agenda-list)
(global-set-key [f10]  'helm-org-rifle)
(global-set-key [f11]  (lambda () (interactive) (switch-to-buffer "*dashboard*")))
(global-set-key [f12]  'org-capture)

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
(global-set-key (kbd "C-c o") 'helm-find-files)
(global-set-key (kbd "C-z") 'undo)

;;  (use-package exwm
;;   :init
;;    (require 'exwm)
;;    (require 'exwm-config)
;;    (exwm-config-default))



(put 'narrow-to-region 'disabled nil)
