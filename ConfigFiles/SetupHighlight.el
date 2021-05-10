;; All the highlight stuff config

;; highlight-symbol: move to next/prev occurrences of symbol + highlight
;; https://github.com/nschum/highlight-symbol.el
(use-package highlight-symbol
  :config (highlight-symbol-nav-mode)
  )

;; volatile-highlights: highlight specific operations like undo, yank
;; https://github.com/k-talo/volatile-highlights.el
(use-package volatile-highlights
  :hook
  (after_init . volatile_highlights_mode)
  :config
  ;; (volatile-highlights-mode t)
  (vhl/define-extension 'undo-tree 'undo-tree-yank 'undo-tree-move)
  (vhl/install-extension 'undo-tree)
  (vhl/default-face ((nil (:foreground "#FF3333"
			   :background "#FFCDCD"))))
  )

(use-package highlight-indent-guides
  :custom
  (highlight-indent-guides-auto-enabled t)
  (highlight-indent-guides-responsive t)
  (highlight-indent-guides-method 'character))

;; enable hl-line-mode globally
(global-hl-line-mode)

(use-package rainbow-mode
  :hook
  ((emacs-lisp-mode . rainbow-mode)
   (helpful-mode . rainbow-mode))
)

;; highlight-numbers: fontify numbers
;; https://github.com/Fanael/highlight-numbers
(use-package highlight-numbers
  :hook
  ((prog-mode . highlight-numbers-mode))
)

;; hl-todo: Highlight TODO keywords
;; https://github.com/tarsius/hl-todo/tree/master
(use-package hl-todo
  :hook
  (after-init . global-hl-todo-mode)
  )

;; Highlight escape sequences in Emacs
;; https://github.com/dgutov/highlight-escape-sequences/
(use-package highlight-escape-sequences
  :config (hes-mode))

(provide 'SetupHighlight)
