;; vterm: Emacs libvterm integration
;; https://github.com/akermu/emacs-libvterm

(use-package vterm
  :if (executable-find "cmake")
  :bind (
         ("C-c t t" . vterm)
         :map vterm-mode-map
              ("C-y" . vterm-yank))
  :config
  ;; disable some unnecessary minor-modes in term-mode
  (add-hook 'vterm-mode-hook (lambda ()
                               ;;(yas-minor-mode -1)
                               (setq-local global-hl-line-mode nil)
                               
                               ;; Prevent premature horizontal scrolling
                               (setq-local hscroll-margin 0)))

  ;; vterm buffers are killed when the associated process is terminated
  (setq vterm-kill-buffer-on-exit t)
  )

;; vterm-toggle: toggles between the vterm buffer and whatever buffer you are editing.
;; https://github.com/jixiuf/vterm-toggle
(use-package vterm-toggle
  :if (executable-find "cmake")
  :after vterm)

;; multi-vterm: manage multiple terminal windows easily within emacs
;; https://github.com/suonlight/multi-vterm
(use-package multi-vterm
  :after vterm
  :if (executable-find "cmake")
 )

(provide 'SetupVterm)
