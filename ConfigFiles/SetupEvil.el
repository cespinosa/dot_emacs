;; Evil Config

(use-package evil
  :config
  (evil-mode 1)
  (use-package evil-org
    :config
    (evil-org-set-key-theme
     '(textobjects insert navigation additional shift todo heading))
    (add-hook 'org-mode-hook (lambda () (evil-org-mode)))
    )
  )

(provide 'SetupEvil)
