;; LaTeX

(defvar my/bib-file-location "~/GoogleDrive/fractaliusfciencias/Bib/library.bib" "Where I keep my bib file")

(use-package tex-site
  :straight auctex
  :mode ("\\.tex\\'" . latex-mode)
  :config
  (setq TeX-parse-self t)
  (setq TeX-auto-save t)
  (setq-default TeX-master nil)
  (add-hook 'LaTeX-mode-hook
            (lambda ()
              ;;(setq TeX-command-default "latexmk")
              (rainbow-delimiters-mode)
              (company-mode)
              (smartparens-mode)
              (turn-on-reftex)
              (setq reftex-plug-into-AUCTeX t)
              (reftex-isearch-minor-mode)
              (setq TeX-PDF-mode t)
              (setq TeX-source-correlate-method 'synctex)
              (setq TeX-source-correlate-start-server t)))
;;  (add-to-list 'TeX-view-program-selection
;;               '(output-pdf "Zathura"))
;; Update PDF buffers after successful LaTeX runs
  (add-hook 'TeX-after-TeX-LaTeX-command-finished-hook
          #'TeX-revert-document-buffer)
  ;; to use pdfview with auctex
  (add-hook 'LaTeX-mode-hook 'pdf-tools-install)
  (setq TeX-view-program-selection '((output-pdf "pdf-tools"))
        TeX-source-correlate-start-server t)
  (setq TeX-view-program-list '(("pdf-tools" "TeX-pdf-tools-sync-view")))
)

(use-package ivy-bibtex
  :ensure t
  :bind*
  ("C-c M-c C-r" . ivy-bibtex)
  :config
  (setq bibtex-completion-additional-search-fields '(journal booktitle))
  (setq bibtex-completion-display-formats
        '((article       . "${=has-pdf=:1}${=has-note=:1} ${=type=:3} ${year:4} ${author:36} ${title:*} ${journal:40}")
          (inbook        . "${=has-pdf=:1}${=has-note=:1} ${=type=:3} ${year:4} ${author:36} ${title:*} Chapter ${chapter:32}")
          (incollection  . "${=has-pdf=:1}${=has-note=:1} ${=type=:3} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
          (inproceedings . "${=has-pdf=:1}${=has-note=:1} ${=type=:3} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
          (t             . "${=has-pdf=:1}${=has-note=:1} ${=type=:3} ${year:4} ${author:36} ${title:*}")))
  (setq bibtex-completion-bibliography my/bib-file-location)
  (setq bibtex-completion-notes-path "~/GoogleDrive/fractaliusfciencias/Bib/ref.org")
  (setq bibtex-completion-pdf-field "File")
  (setq bibtex-completion-library-path '("~/GoogleDrive/fractaliusfciencias/Bib/Papers" "~/Google_Drive/fractaliusfciencias/Bib/Libros"))
  (setq ivy-bibtex-default-action #'ivy-bibtex-insert-citation)
  (ivy-set-actions
   'ivy-bibtex
   '(("p" ivy-bibtex-open-any "Open PDF, URL, or DOI")
     ("e" ivy-bibtex-edit-notes "Edit notes")
     ("P" ivy-bibtex-open-pdf "Open PDF file (if present)"))
   )
  ;; (defun bibtex-completion-open-pdf-external (keys &optional fallback-action)
  ;;   (let ((bibtex-completion-pdf-open-function
  ;;          (lambda (fpath) (start-process "evince" "*helm-bibtex-evince*" "/usr/bin/evince" fpath))))
  ;;     (bibtex-completion-open-pdf fallback-action)))
  ;; (ivy-bibtex-ivify-action bibtex-completion-open-pdf-external ivy-bibtex-open-pdf-external)
  ;; (ivy-add-actions
  ;;  'ivy-bibtex
  ;;  '(("P" ivy-bibtex-open-pdf-external "Open PDF file in external viewer (if present)")))
  (setq bibtex-completion-pdf-symbol "⌘")
  (setq bibtex-completion-notes-symbol "✎")
)

(use-package pdf-tools
  :ensure t
  :mode ("\\.pdf\\'" . pdf-tools-install)
  :bind ("C-c M-c C-g" . pdf-sync-forward-search)
  :defer t
  :config
  (setq mouse-wheel-follow-mouse t)
  (setq pdf-view-resize-factor 1.10))

;; writing tool
(use-package academic-phrases :ensure t)

(use-package reftex
  :ensure t
  :defer t
  :config
  (setq reftex-cite-prompt-optional-args t)
) 

(use-package synosaurus
  :diminish synosaurus-mode
  :init    (synosaurus-mode)
  :config  (setq synosaurus-choose-method 'default) ;; 'ido is default.
  (global-set-key (kbd "C-c M-c s") 'synosaurus-choose-and-replace)
)

(setq wordnut-cmd "/usr/local/WordNet-3.0/bin/wn")

(use-package wordnut
  :bind ("C-c M-c w" . wordnut-lookup-current-word))

(provide 'SetupLatex)
