;; Quitting emacs via `C-x C-c` or the GUI 'X' button
(setq confirm-kill-emacs #'y-or-n-p)

;; Set email address
(setq user-mail-address "cespinosa@astro.unam.mx")

(provide 'general)
