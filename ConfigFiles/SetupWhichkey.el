;; which-key : show popup of keybindings starting with a prefix
;; https://github.com/justbur/emacs-which-key

(use-package which-key
  :init (which-key-mode)
  :hook (after-init . which-key-mode)
  )

(provide 'SetupWhichkey)
