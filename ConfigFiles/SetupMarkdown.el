(use-package markdown-mode
  :mode ("/README\\(?:\\.\\(?:markdown\\|md\\)\\)?\\'" . gfm-mode)
  :hook ((markdown-mode . (lambda ()
                            (setq-local display-line-numbers-type 'absolute)
                            )
                        )
         )
  :config
  (setq markdown-enable-wiki-links t
        markdown-italic-underscore t
        markdown-enable-math t ; syntax highlighting for latex fragments
        markdown-list-indent-width 2
        markdown-hide-urls nil ; trigger with `markdown-toggle-url-hiding'
        markdown-fontify-code-blocks-natively t
        markdown-gfm-uppercase-checkbox t) ; for compat with org-mode
  ;; Don't change font in code blocks
  (set-face-attribute 'markdown-code-face nil
                      :inherit nil)
  )

;; markdown-toc: generate table of contents for markdown
;; https://github.com/ardumont/markdown-toc
(use-package markdown-toc
  :after markdown-mode)

(provide 'SetupMarkdown)

