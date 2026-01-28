(defun my-avy-goto-line ()
  "Avy goto line plus move the cursor to the end of line"
  (interactive)
  (progn (avy-goto-line)
         (move-end-of-line nil)))


(defun scratch ()
  "create a new scratch buffer to work in. (could be *scratch* - *scratchX*).
   Prompts for major mode. Currently supported modes are
   none
   json
   elisp"
  (interactive)
  (let ((option (completing-read "Choose an option: "
                                 '("none" "json" "elisp"))))
    (let ((n 0)
          bufname)
      (while (progn
               (setq bufname (concat "*scratch"
                                     (if (= n 0) "" (int-to-string n))
                                     "*"))
               (setq n (1+ n))
               (get-buffer bufname)))
      (switch-to-buffer (get-buffer-create bufname))
      (if (= n 1) initial-major-mode)
      (pcase option
            ("json" (json-mode))
            ("elisp" (emacs-lisp-mode))))))

;; Python related


(defun my-python-import-name-at-point ()
  "Try to import whatever is under cursor"
  (interactive)
  (python-add-import (current-word)))

(defun my-python-pytest-repeat ()
  "Save all buffers and repeat test"
  (interactive)
  (projectile-save-project-buffers)
  (python-pytest-repeat))
(defun my-python-pytest-function ()
  "Save all buffers and run current function"
  (interactive)
  (projectile-save-project-buffers)
  (python-pytest-function (buffer-file-name)
                          (python-pytest--current-defun)
                          (transient-args 'python-pytest-dispatch)))

(defun my-python-pytest-file ()
  "Save all buffers and run current file"
  (interactive)
  (projectile-save-project-buffers)
  (python-pytest-file (buffer-file-name)))


(defun my-current-project-root ()
  (interactive)
  (vc-root-dir))

(defun my-python-lint-done (process signal)
  (when (memq (process-status process) '(exit signal))
    (with-current-buffer "*Python Lint*"
      (read-only-mode t)
      (my-buttonize-buffer)
      (special-mode)
      (shell-command-sentinel process signal))))


(defun my-python-lint ()
  "Lint code"
  (interactive)
  (projectile-save-project-buffers)
  (let ((default-directory (projectile-project-root)));; This should work but it does not ?(vc-root-dir)))
    (let* ((output-buffer (get-buffer-create "*Python Lint*"))
           (proc (progn
                   (async-shell-command "make lint" output-buffer)
                   (get-buffer-process output-buffer))))
      (if (process-live-p proc)
          (set-process-sentinel proc #'my-python-lint-done)
        (message "No process running."))))

  (mapcar (lambda (b) (if (buffer-modified-p b) (revert-buffer))) (projectile-project-buffers)))






(defun find-file-button (button)
  (find-file-other-window (buffer-substring (button-start button) (button-end button))))

(define-button-type 'find-file-button
  'follow-link t
  'action #'find-file-button)



(defun my-buttonize-buffer ()
  "Turn all file paths and URLs into buttons."
  (interactive)
  (require 'ffap)
  (deactivate-mark)
  (let (token guess beg end reached bound len)
    (save-excursion
      (setq reached (point-min))
      (goto-char (point-min))
      (while (re-search-forward ffap-next-regexp nil t)
        ;; There seems to be a bug in ffap, Emacs 23.3.1: `ffap-file-at-point'
        ;; enters endless loop when the string at point is "//".
        (setq token (ffap-string-at-point))
        (unless (string= "//" (substring token 0 2))
          ;; Note that `ffap-next-regexp' only finds some "indicator string" for a
          ;; file or URL. `ffap-string-at-point' blows this up into a token.
          (save-excursion
            (beginning-of-line)
            (when (search-forward token (point-at-eol) t)
              (setq beg (match-beginning 0)
                    end (match-end 0)
                    reached end))
            )
          (message "Found token %s at (%d-%d)" token beg (- end 1))
          ;; Now let `ffap-guesser' identify the actual file path or URL at
          ;; point.
          (when (setq guess (ffap-guesser))
            (message "  Guessing %s" guess)
            (save-excursion
              (beginning-of-line)
              (when (search-forward guess (point-at-eol) t)
                (setq len (length guess) end (point) beg (- end len))
                ;; Finally we found something worth buttonizing. It shall have
                ;; at least 2 chars, however.
                (message "    Matched at (%d-%d]" beg (- end 1))
                (unless (or (< (length guess) 2))
                  (message "      Buttonize!")
                  (make-button beg end :type 'find-file-button))
                )))
          ;; Continue one character after the guess, or the original token.
          (goto-char (max reached end))
          (message "Continuing at %d" (point))
          )))))


(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's iting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))



(defvar ffap-file-at-point-line-number nil
  "Variable to hold line number from the last `ffap-file-at-point' call.")

(defadvice ffap-file-at-point (after ffap-store-line-number activate)
  "Search `ffap-string-at-point' for a line number pattern and
save it in `ffap-file-at-point-line-number' variable."
  (let* ((string (ffap-string-at-point)) ;; string/name definition copied from `ffap-string-at-point'
         (name
          (or (condition-case nil
                  (and (not (string-match "//" string)) ; foo.com://bar
                       (substitute-in-file-name string))
                (error nil))
              string))
         (line-number-string
          (and (string-match ":[0-9]+" name)
               (substring name (1+ (match-beginning 0)) (match-end 0))))
         (line-number
          (and line-number-string
               (string-to-number line-number-string))))
    (if (and line-number (> line-number 0))
        (setq ffap-file-at-point-line-number line-number)
      (setq ffap-file-at-point-line-number nil))))

(defadvice find-file-at-point (after ffap-goto-line-number activate)
  "If `ffap-file-at-point-line-number' is non-nil goto this line."
  (when ffap-file-at-point-line-number
    (forward-line ffap-file-at-point-line-number)
    (setq ffap-file-at-point-line-number nil)))

;; TODO This doesnt overried automatically
(defun dap-python--pyenv-executable-find (command)
    (executable-find command))

(defun my-dap-delete-all-breakpoints()
  (interactive)
  (dap-breakpoint-delete-all))

(defun replace-keywords-in-string (str replacements)
  "Replace keywords in STR with their corresponding values from REPLACEMENTS.
   REPLACEMENTS is a list of cons pairs where the car is the keyword
   (without %( and )s) and the cdr is the replacement value."
    (cl-reduce (lambda (result pair)
               (let ((keyword (car pair))
                     (replacement (cdr pair)))
                 (replace-regexp-in-string (format "%%(%s)s" (regexp-quote keyword)) replacement result)))
             replacements
             :initial-value str))


;; LSP extras

;; Compile
;;

;;
;; Calendar stuff
;;

(defun my-calendar-add-iso-date ()
  "Select date and time from calendar and add it as iso timestamp"
    (interactive)
      (let ((time (org-read-date nil 'to-time nil "Date:  ")))
    (insert (format-time-string "%Y-%m-%dT%H:%M:%S%z" time))))


(defun chmod-buffer-user-executable ()
  "Make current buffer user executable"
   (interactive)
    (let* ((current-buffer (buffer-file-name))
           (modes (or (if current-buffer (file-modes current-buffer) 0)
                  (error "File not found. Not saved?"))))
      (chmod current-buffer (file-modes-symbolic-to-number "u+x" modes) )))

(defun my-load-z-agenda ()
  (interactive)
  (org-agenda nil "a"))


(defun my-homescreen ()
  "Just load the agenda week view"
  (interactive)
  (my-load-z-agenda)
  (delete-other-windows)
)

(defun my-rgrep-thing-selection-project (word)
  "Find WORD which defaults to symbol under cursor in current project"
  (interactive (list (read-string (format "Find word (default: %s): " (thing-at-point 'symbol))
                                  (thing-at-point 'symbol))))
  (let ((project-dir (my-current-project-root)))
    (if project-dir
        (rgrep word "*.*" project-dir))))


(defun my-function-with-word-or-prompt ()
  "An example function that uses the word under the cursor as its parameter, but prompts to edit it if needed."
  (interactive
   (let ((word (thing-at-point 'word)))
     (list (read-string (format "Use word (default: %s): " word) nil nil word))))
  (message "The word used is: %s" word))



(defun my-sync-org-git (&optional predefined-answer)
  "Execute push.sh with an optional predefined answer. If predefined-answer is non-nil, it should be 'yes or 'no."
 (interactive)
  (let ((run-push (if predefined-answer
                      (eq predefined-answer 'y)
                    (y-or-n-p "Do you want to run push.sh? "))))
    (if run-push
        (shell-command "bash ~/Org/sync.sh"))))

(run-with-timer 0 (* 5 60) (lambda () (my-sync-org-git "y")))


(defun copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

;;; AI

;;(let ((ai-el (expand-file-name "ai.el" user-emacs-directory)))
;;  (when (file-exists-p ai-el)
;;    (load ai-el)))



;;; Gtasks
;; org-gtasks-accounts
(defun my-org-gtasks-sync-all ()
  "Synchronize all accounts when org-gtasks-accounts is a list of hash tables."
  (interactive)
  (if (and (boundp 'org-gtasks-accounts) (listp org-gtasks-accounts))
      (dolist (account org-gtasks-accounts)
	(org-gtasks-pull account))))



(define-advice basic-save-buffer-2 (:around (orig-fun &rest args) try-sudo-save)
  "If the file is owned by root or not writable, try to save with sudo."
  (let* ((filename (buffer-file-name))
         ;; Check if file exists and is owned by root (UID 0)
         (is-root-owned (and filename
                             (file-exists-p filename)
                             (zerop (file-attribute-user-id (file-attributes filename))))))

    (if (and filename
             ;; Trigger if: Root owned OR Not writable
             (or is-root-owned (not (file-writable-p filename)))
             ;; Safety: Don't trigger if already remote (to avoid recursion)
             (not (file-remote-p filename)))

        ;; --- Sudo Save Logic ---
        (let ((old-buffer-file-name buffer-file-name)
              (success nil))
          (unwind-protect
              (progn
                ;; Temporarily change filename to TRAMP sudo path
                (setq buffer-file-name (concat "/sudo::" filename))
                ;; Call the original save-buffer function
                (apply orig-fun args)
                (setq success t))
            
            ;; Cleanup: Restore filename
            (setq buffer-file-name old-buffer-file-name)
            ;; Revert buffer to update read-only status and permissions
            (when success
              (revert-buffer t t))))

      ;; --- Normal Save Logic ---
      ;; If not root/protected, just run the original function normally
      (apply orig-fun args))))
