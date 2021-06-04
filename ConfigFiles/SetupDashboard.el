;; Dashboard condif

(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-center-content t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-navigator t)
  (setq dashboard-week-agenda t)
  (setq dashboard-items '((recents  . 5)
                        (bookmarks . 5)
                        (projects . 5)
                        (agenda . 5)
                        (registers . 5)))
  ;; (add-to-list 'dashboard-items '(agenda) t)
  (setq dashboard-org-agenda-categories '("Tasks" "Appointments"))

  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  )

(provide 'SetupDashboard)
