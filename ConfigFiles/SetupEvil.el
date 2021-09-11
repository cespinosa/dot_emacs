;; Evil Config

(use-package evil
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)

  (use-package evil-surround
    :config
    (global-evil-surround-mode))
  
  (use-package evil-org
    :config
    (evil-org-set-key-theme
     '(textobjects insert navigation additional shift todo heading))
    (add-hook 'org-mode-hook (lambda () (evil-org-mode)))
    )
  )

;; (use-package evil-nerd-commenter
;;   )

(use-package evil-commentary
  :config
  (evil-commentary-mode)
  )

(use-package evil-collection
  :after evil
  :config
  (setq evil-collection-mode-list '(dashboard dired ibuffer))
  (evil-collection-init)
  )

(provide 'SetupEvil)
