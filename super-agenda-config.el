(use-package org-super-agenda
  :config (org-super-agenda-mode t)
  (setq org-agenda-remove-tags t)
  (add-hook 'org-agenda-mode-hook (lambda () (setq show-trailing-whitespace nil))))

(use-package htmlize)

(defun my-org-super-agenda-work-groups ()
  (mapcar (lambda (p) `(:name ,(capitalize p) :tag ,p :order 2))
          (mapcan 'identity (org-map-entries (lambda () (org-get-tags (point) t)) "LEVEL=2" '("~/Org/Agenda/projects.org")))))


(defun timestamp-today-string-plus-n-days (n)
        "Get the timestamp of n days after today."
        (format-time-string "%Y-%m-%d" (time-add (* 60 60 24 n) (current-time))))

(setq org-super-agenda-groups
      (append (my-org-super-agenda-work-groups)
      `((:name "Today"
               :time-grid t
               :date today
               :deadline today
               :order 1)
        (:name "Next 3 days"
               :deadline (before ,(timestamp-today-string-plus-n-days 3))
               :order 9)
        (:name "Important"
               :tag "Important"
               :priority "A"
               :order 6)
        (:name "Work"
               :tag "work"
               :order 4)
        (:name "Overdue"
               :face (:weight :bold-light)
               :deadline past
               :order 7)
        (:name "Personal"
               :deadline (before ,(timestamp-today-string-plus-n-days 3))
               :tag "personal"
               :face (:foreground "WhitemSoke")
               :order 20)
        (:name "Trivial"
               :priority<= "C"
               :tag ("Trivial" "Unimportant")
               :todo ("SOMEDAY" )
               :order 90)
        (:discard (:tag ("chore" "routine" "daily" ))))))

(setq org-agenda-custom-commands
      `(("D" agenda "Export Summary"
         ((ps-number-of-columns 3)
          (ps-landscape-mode t)
          (org-agenda-span 3) ;; Number of days th
          (org-agenda-prefix-format "%t %s")
          (org-agenda-time-grid t)
          (org-agenda-day-face-function (lambda (date) 'org-agenda-date))
          (org-agenda-overriding-header "\nToday's agenda\n")
          (org-agenda-fontify-priorities nil)
          (org-agenda-current-time-string ,(car (last org-agenda-time-grid)))
          (org-agenda-remove-tags t))
         ,(expand-file-name "~/Documents/agenda-classic.html"))
        ("z" "Daily TODO"
          (alltodo "" ((org-agenda-remove-tags t)
                       (org-agenda-prefix-format "%t %s")
                       (org-agenda-overriding-header "")
                       (org-super-agenda-groups
                        )
         "" ,(expand-file-name "~/Documents/agenda.html"))))))


