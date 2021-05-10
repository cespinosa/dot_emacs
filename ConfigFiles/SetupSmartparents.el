;; smartparens: for movement, editing and inserting parenthesis
;; https://github.com/Fuco1/smartparens

(use-package smartparens
  :hook
  (after-init . smartparens-global-mode)
  :config
  (setq sp-ignore-modes-list (quote (minibuffer-inactive-mode
                                     Info-mode
                                     term-mode
                                     org-mode
                                     org-journal-mode
                                     markdown-mode 
                                     ivy-occur-mode)))
  (defmacro def-pairs (pairs)
    `(progn
       ,@(cl-loop for (key . val) in pairs
                  collect
                  `(defun ,(read (concat
                                  "wrap-with-"
                                  (prin1-to-string key)
                                  "s"))
                       (&optional arg)
                     (interactive "p")
                     (sp-wrap-with-pair ,val)))))
  (def-pairs ((paren . "(")
              (bracket . "[")
              (brace . "{")
              (single-quote . "'")
              (double-quote . "\"")
              (back-quote . "`")))
  
  ;; (smartparens-global-strict-mode)
  ;; (show-smartparens-global-mode +1)
  (require 'smartparens-config)
  (sp-pair "=" "=" :actions '(wrap))
  (sp-pair "+" "+" :actions '(wrap))
  (sp-pair "<" ">" :actions '(wrap))
  (sp-pair "$" "$" :actions '(wrap))
  )

(provide 'SetupSmartparents)
