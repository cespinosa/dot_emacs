;; flycheck configuration


;; flycheck: on the fly syntax checking
;; http://www.flycheck.org/en/latest/
(use-package flycheck
  :hook
  (python-mode . flycheck-mode)
  )

(provide 'SetupFlycheck)
