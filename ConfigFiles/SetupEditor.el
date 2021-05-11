;;; configuration for all the editing stuff in emacs

;; cutting and pasting uses primary clipboard
(setq select-enable-primary t)
(setq select-enable-clipboard t)

;; always insert spaces, do not insert tabs
(setq-default indent-tabs-mode nil)
;; set default tab width to 4
(setq-default tab-width 4)

(setq-default fill-column 80) ;; default is 70

;; Delete selection if insert someting
(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))

;; Hungry deletion
(use-package hungry-delete
  :diminish
  :hook (after-init . global-hungry-delete-mode)
  :config (setq-default hungry-delete-chars-to-skip " \t\f\v"))

(provide 'SetupEditor)
