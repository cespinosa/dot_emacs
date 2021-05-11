 ;; Python config

(setq python-shell-interpreter "/home/espinosa/anaconda3/bin/python3")

(use-package conda
  :init
  (setq conda-anaconda-home "/home/espinosa/anaconda3/")
  (setq conda-default-env "base")
  :hook
  (python-mode . (lambda () (conda-env-activate "base")))
  ;;(python-mode . conda-env-autoactivate-mode)
  )

(use-package py-autopep8
  :hook
  (python-mode . py-autopep8-enable-on-save)
  )

(provide 'SetupPython)
