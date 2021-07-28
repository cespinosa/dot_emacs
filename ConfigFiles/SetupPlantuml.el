;; A major mode for editing PlantUML sources in EMACS
;; https://github.com/skuro/plantuml-mode

(use-package plantuml-mode
  :mode ("\\.plantuml\\'" . plantuml-mode)
  :hook ((plantuml-mode . flycheck-mode)
         (plantuml-mode . company-mode))
  :config

  ;; Sample executable configuration
  ;; Make sure plantuml is installed and PATH is configured to point to plantuml
  ;; binary file
  (setq plantuml-executable-path (executable-find "plantuml"))
  (setq plantuml-default-exec-mode 'executable)

  ;; path for the plantuml jar file
  (setq plantuml-jar-path "~/.emacs.d/plantuml.jar"))

;; flycheck plantuml checker
;; https://github.com/alexmurray/flycheck-plantuml
(use-package flycheck-plantuml
  :after plantuml-mode
  :config (flycheck-plantuml-setup))

(provide 'SetupPlantuml)
