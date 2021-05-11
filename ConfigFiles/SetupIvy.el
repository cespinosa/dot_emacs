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

(provide 'SetupIvy)
