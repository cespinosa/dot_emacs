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

;; Group ibuffer's list by project root
(use-package ibuffer-projectile
  :functions all-the-icons-octicon ibuffer-do-sort-by-alphabetic
  :hook ((ibuffer . (lambda()
                      (ibuffer-projectile-set-filter-groups)
                      (unless (eq ibuffer-sorting-mode 'alphabetic)
                        (ibuffer-do-sort-by-alphabetic)))))
  :config
  (setq ibuffer-projectile-prefix
        (concat
         (all-the-icons-octicon "file-directory"
                                :face ibuffer-filter-group-name-face
                                :v-adjust 0.0
                                :height 1.0)
         " "
          "Project: "))
  )

(provide 'SetupIBuffer)
