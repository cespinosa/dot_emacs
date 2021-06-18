;; https://magit.vc , https://github.com/magit/magit
;; magit: the git porcelain to manage git

(use-package magit
  :config
  ;; Refresh `magit-status' after saving a buffer
  (add-hook 'after-save-hook #'magit-after-save-refresh-status)
  )

(use-package git-timemachine
  )

(use-package diff-hl
  :bind
  ("C-c g s" . magit-status)
  :init
  (global-diff-hl-mode)
  )

(use-package magit-gitflow
  :hook (magit-mode . turn-on-magit-gitflow)
  )

(provide 'SetupGit)
