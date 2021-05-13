;; Set font only if it available
(defun rag-set-face (frame)
  "Configure faces on frame creation"
  (select-frame frame)
  (if (display-graphic-p)
      (when (member "FiraCode Nerd Font" (font-family-list))
        (set-frame-font "FiraCode Nerd Font 12" nil t))
      )
  )

(add-hook 'after-make-frame-functions #'rag-set-face)

;; set frame font when running emacs normally
(when (member "FiraCode Nerd Font" (font-family-list))
  (set-frame-font "FiraCode Nerd Font 12" nil t))

(provide 'SetupFont)
