;; Quitting emacs via `C-x C-c` or the GUI 'X' button
(setq confirm-kill-emacs #'y-or-n-p)

;; ALias yes or no
(defalias 'yes-or-no-p #'y-or-n-p)

(setq frame-title-format "Emacs")
(setq ring-bell-function 'ignore)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets) ; Show path if names are same
(setq adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*")
(setq adaptive-fill-first-line-regexp "^* *$")
(setq sentence-end "\\([。、！？]\\|……\\|[,.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
(setq sentence-end-double-space nil)
(setq delete-by-moving-to-trash t)    ; Deleting files go to OS's trash folder
(setq make-backup-files nil)          ; Forbide to make backup files
(setq auto-save-default nil)          ; Disable auto save
(setq set-mark-command-repeat-pop t)  ; Repeating C-SPC after popping mark pops it again
(setq track-eol t)			; Keep cursor at end of lines.
(setq line-move-visual nil)		; To be required by track-eol
(setq-default kill-whole-line t)	; Kill line including '\n'
(setq-default indent-tabs-mode nil)   ; use space

;; Set email address
(setq user-mail-address "cespinosa@astro.unam.mx")

(provide 'general)
