;; amx: An alternative M-x interface for Emacs.
;; https://github.com/DarwinAwardWinner/amx
(use-package amx :defer 0.5
  :config (amx-mode))

;; swiper: isearch with an overview!
;; https://github.com/abo-abo/swiper
;; `M-p' -> previous search item
;; `M-n' -> next search item
;; `M-n' -> to select the symbol at point in swiper
(use-package swiper
  :bind (("C-s" . swiper-isearch)
         ("M-s ." . swiper-isearch-thing-at-point))
  :config
  (setq swiper-action-recenter t
        ;; Jump to the beginning of match when leaving Swiper
        swiper-goto-start-of-match t))

;; ivy: incremental narrowing framework for Emacs
;; https://github.com/abo-abo/swiper
(use-package ivy
  :bind
  (("C-c u" . ivy-resume))
  :config
  (ivy-mode)

  (bind-chords
   :map ivy-minibuffer-map
   ("m," . ivy-beginning-of-buffer)
   (",." . ivy-end-of-buffer))

  (setq ivy-use-virtual-buffers t
        ivy-height 13
        ivy-count-format "%d/%d "
        ivy-virtual-abbreviate 'full ; show the full virtual paths
        ivy-extra-directories nil ; default value: ("../" "./")
        ivy-wrap t
        ivy-action-wrap t
        ivy-use-selectable-prompt t)
  
  ;; modify default search behaviour of ivy
  (setq ivy-re-builders-alist
        '((t . ivy--regex-plus)))

  (bind-keys
   :map ivy-occur-grep-mode-map
   ("n" . ivy-occur-next-line)
   ("p" . ivy-occur-previous-line)
   ("b" . backward-char)
   ("f" . forward-char)
   ("v" . ivy-occur-press) ; default f
   ("RET" . ivy-occur-press)
   )
  )

(use-package all-the-icons-ivy-rich
  :defer 1
  :config
  (all-the-icons-ivy-rich-mode 1)
  (setq all-the-icons-ivy-rich-icon-size 0.8)
  )

(use-package ivy-rich
  :hook (counse-mode . ivy-rich-mode)
  :config
  ;; For better performance
  ;; Better experience with icons
  (setq ivy-rich-parse-remote-buffer nil)
  )

(provide 'SetupIvy)
