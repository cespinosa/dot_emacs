
;; This program provides some interactive functions which allows users
;; to transpose windows arrangement in currently selected frame
;; https://github.com/emacsorphanage/transpose-frame/blob/master/transpose-frame.el
(use-package transpose-frame)

;; ace-window: quick switching of windows
;; https://github.com/abo-abo/ace-window
(use-package ace-window
  :commands (ace-window)
  :bind
  ("C-c w" . ace-window)
  :custom
  (aw-keys '(?j ?k ?l ?i ?o ?h ?y ?u ?p))
  :config
  ;; enable aw dispatch even for just one window
  (setq aw-dispatch-always t)

  ;; enable the ace-window number in mode-line
  (ace-window-display-mode)
  )

(provide 'SetupWindows)
