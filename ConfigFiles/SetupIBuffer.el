;; ibuffer: for esay management of buffers
(use-package ibuffer
  :straight nil
  :init (setq ibuffer-filter-group-name-face
              '(:inherit (font-lock-type-face bold)))
  :config

  ;; Don't show scratch and messages in iBuffer
  (require 'ibuf-ext)
  (add-to-list #'ibuffer-never-show-predicates "^\\*Messages")
  (add-to-list #'ibuffer-never-show-predicates "^\\*Scratch")
  (add-to-list #'ibuffer-never-show-predicates "^\\*Bookmark List")
  ;; hide filter groups which are empty
  (setq ibuffer-show-empty-filter-groups nil)  

  (bind-key "C-c b i" #'ibuffer)

  ;; dont ask for confirmation whenever killing a buffer
  (setq ibuffer-expert t)
  )

;; Display icons for all buffers in ibuffer.
;; https://github.com/seagle0128/all-the-icons-ibuffer
(use-package all-the-icons-ibuffer
  :config
  ;; enable ibuffer all-the-icons support
  (all-the-icons-ibuffer-mode 1)
  )

(provide 'SetupIBuffer)
