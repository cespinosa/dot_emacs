;; https://github.com/Fanael/rainbow-delimiters
;; different colours for each nested delimiter
(use-package rainbow-delimiters
  :hook
  ((prog-mode . rainbow-delimiters-mode)))

;; show line numbers globally
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)

;; Easily adjust the font size in all Emacs frames
;; https://github.com/purcell/default-text-scale/
(use-package default-text-scale
  :defer 5
  :config (default-text-scale-mode))

;; turn on the native fill column indicator
;; requires emacs27 or newer
(use-package display-fill-column-indicator
  :straight nil
  :hook
  ((prog-mode
    conf-mode
    org-mode) . display-fill-column-indicator-mode)
  :config
  (setq-default display-fill-column-indicator-character 80)
  (setq-default display-fill-column-indicator-character ?|)
  )

;; built in mode `image-mode' for viewing images
(use-package image
  :straight nil
  ;; disable line numbers for images
  :hook (image-mode . (lambda ()
                        (display-line-numbers-mode -1))))

(use-package nyan-mode
  :custom
  (nyan-cat-face-number 4)
  (nyan-animate-nyancat t)
  :hook
  (doom-modeline-mode . nyan-mode)
  )

(provide 'SetupVisual)
