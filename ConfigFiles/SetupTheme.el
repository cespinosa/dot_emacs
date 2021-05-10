;; function to disable all enabled themes
(defun es/disable-all-themes ()
  (interactive)
  (mapc #'disable-theme custom-enabled-themes))

;;; Theme hooks
(defvar es/theme-hooks nil
  "((theme-id . function) ...)")

(defun es/add-theme-hook (theme-id hook-func)
  (add-to-list 'gh/theme-hooks (cons theme-id hook-func)))

(defun es/load-theme-advice (f theme-id &optional no-confirm no-enable &rest args)
  "Enhances `load-theme' in two ways:
1. Disables enabled themes for a clean slate.
2. Calls functions registered using `es/add-theme-hook'."
  (unless no-enable
    (es/disable-all-themes))
  (prog1
      (apply f theme-id no-confirm no-enable args)
    (unless no-enable
      (pcase (assq theme-id es/theme-hooks)
        (`(,_ . ,f) (funcall f))))))

(advice-add 'load-theme
            :around
            #'es/load-theme-advice)

;; Doom theme
(use-package doom-themes
  :custom
  (doom-themes-enable-italic t)
  (doom-themes-enable-bold t)
  :config
  (load-theme 'doom-dracula t)
  ;; Corrects (and improves) org-mode's native fontification.  
  (doom-themes-org-config)

  ;; use my font instead of the default variable pitch font used by
  ;; doom-themes-treemacs-theme  
  (setq doom-themes-treemacs-enable-variable-pitch nil)
  ;; Modeline
  (use-package doom-modeline
    :hook
    (after-init . doom-modeline-mode)
  )
)

(provide 'SetupTheme)
