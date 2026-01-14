(require 'json)
(require 'url)

(defun my-sanitize-string (str)
  "Convert ä->a, ö->o, å->a and replace non-alphanumerics with underscores."
  (let* ((result (downcase str))
         ;; Replace Nordic characters
         (result (replace-regexp-in-string "ä" "a" result))
         (result (replace-regexp-in-string "å" "a" result))
         (result (replace-regexp-in-string "ö" "o" result))
         ;; Replace all other non-alphanumeric chars with underscore
         (result (replace-regexp-in-string "[^a-z0-9]+" "_" result))
         ;; Remove leading/trailing underscores
         (result (replace-regexp-in-string "^_+\\|_+$" "" result)))
    result))

(defun my-magit-new-project-ado ()
  "Fetch active tickets assigned to me from ADO and return a list of (id . sanitized-title)."
  (let* ((auth (base64-encode-string (concat ":" chore-azure-devops-token) t))
         (url-request-extra-headers `(("Authorization" . ,(concat "Basic " auth))
                                      ("Content-Type" . "application/json")))
         (wiql-url (format "https://dev.azure.com/%s/%s/_apis/wit/wiql?api-version=7.1"
                           chore-azure-devops-organization
                           chore-azure-devops-project))
         (wiql-query (json-encode `((query . ,(format "Select [System.Id] From WorkItems Where [System.TeamProject] = '%s' AND [System.AssignedTo] = @Me AND [System.State] = 'Active'" 
                                                      chore-azure-devops-project)))))
         (url-request-method "POST")
         (url-request-data (encode-coding-string wiql-query 'utf-8))
         (ids (with-current-buffer (url-retrieve-synchronously wiql-url)
                (goto-char (point-min))
                (re-search-forward "^$")
                (let* ((data (json-read))
                       (work-items (append (assoc-default 'workItems data) nil)))
                  (mapcar (lambda (item) (cdr (assoc 'id item))) work-items)))))
    
    (if (not ids)
        (message "No active tickets found.")
      (let* ((ids-str (mapconcat #'number-to-string ids ","))
             (details-url (format "https://dev.azure.com/%s/%s/_apis/wit/workitems?ids=%s&fields=System.Id,System.Title&api-version=7.1"
                                  chore-azure-devops-organization
                                  chore-azure-devops-project
                                  ids-str)))
        (setq url-request-method "GET")
        (setq url-request-data nil)
        (with-current-buffer (url-retrieve-synchronously details-url)
          ;; Fix for the encoding error: 
          ;; Use decode-coding-string on the buffer content instead of region manipulation
          (goto-char (point-min))
          (re-search-forward "^$")
          (let* ((json-object-type 'alist)
                 (raw-body (buffer-substring (point) (point-max)))
                 (decoded-body (decode-coding-string raw-body 'utf-8))
                 (data (json-read-from-string decoded-body))
                 (items (append (assoc-default 'value data) nil)))
            (mapcar (lambda (item)
                      (let* ((fields (assoc-default 'fields item))
                             (id (assoc-default 'System.Id fields))
                             (title (assoc-default 'System.Title fields)))
                        (cons (number-to-string id) (my-sanitize-string title))))
                    items)))))))

(defun my-magit-new-project-branch ()
  "Prompt user to select a ticket and return the formatted branch name."
  (let* ((tickets (my-magit-new-project-ado))
         (choices (mapcar (lambda (tkt) 
                            (format "%s: %s" (car tkt) (cdr tkt))) 
                          tickets))
         (selection (completing-read "Select ticket: " choices nil t)))
    (when (and selection (string-match "\\([0-9]+\\): \\(.+\\)" selection))
      (format "features/%s/%s" (match-string 1 selection) (match-string 2 selection)))))

(defun my-magit-get-default-branch ()
  "Determine the default branch by looking at origin/HEAD via symbolic-ref."
  (let ((ref (magit-git-str "symbolic-ref" "refs/remotes/origin/HEAD")))
    (if (and ref (string-match "refs/remotes/origin/\\(.+\\)" ref))
        (match-string 1 ref)
      "master")))

(defun my-magit-create-project-branch (name)
  "Create a branch from the remote's default branch, prompting for the NAME."
  (interactive (list (my-magit-new-project-branch)))
  (when (and name (not (string-empty-p name)))
    (let ((start-point (my-magit-get-default-branch)))
      (magit-branch-and-checkout name start-point))))


;; Add to Magit menu
(with-eval-after-load 'magit-branch
  (transient-append-suffix 'magit-branch "c"
    '("P" "Project branch" my-magit-create-project-branch)))
