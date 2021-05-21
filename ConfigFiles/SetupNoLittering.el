;; Help keeping ~/.emacs.d clean
;; https://github.com/emacscollective/no-littering

(use-package no-littering
  :config

  ;; auto-sabe files in its own directory
  (setq auto-save-file-name-transforms
        `((".*" , (no-littering-expand-var-file-name "auto-save/") t)))

  ;;save custom file to a separate file
  (setq custom-file (no-littering-expand-etc-file-name "custom-settings.el"))
  (load custom-file :noerror :nomessage) ; load custom-file silently
  )

(provide 'SetupNoLittering)
