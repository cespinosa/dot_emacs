;; Org-mode configuration
;; http://orgmode.org/

;;______________________________________________________________________
;;;;  Installing Org with straight.el
;;; https://github.com/raxod502/straight.el/blob/develop/README.md#installing-org-with-straightel
(require 'subr-x)
(straight-use-package 'git)

(defun org-git-version ()
  "The Git version of 'org-mode'.
Inserted by installing 'org-mode' or when a release is made."
  (require 'git)
  (let ((git-repo (expand-file-name
                   "straight/repos/org/" user-emacs-directory)))
    (string-trim
     (git-run "describe"
              "--match=release\*"
              "--abbrev=6"
              "HEAD"))))

(defun org-release ()
  "The release version of 'org-mode'.
Inserted by installing 'org-mode' or when a release is made."
  (require 'git)
  (let ((git-repo (expand-file-name
                   "straight/repos/org/" user-emacs-directory)))
    (string-trim
     (string-remove-prefix
      "release_"
      (git-run "describe"
               "--match=release\*"
               "--abbrev=0"
               "HEAD")))))

(provide 'org-version)

;; (straight-use-package 'org) ; or org-plus-contrib if desired

(use-package ob-async
  :config
  (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)
  )

(defun my-org-screenshot()
  "Take a screenshot into a time stamped unique-named file in the
   same directory as the org-buffer and insert a link to this file."
  (interactive)
  (org-display-inline-images)
  (setq filename
        (concat
         (make-temp-name
          (concat (file-name-nondirectory (buffer-file-name))
                  "_imgs/"
                  (format-time-string "%Y%m%d_%H%M%S_")) ) ".png"))
  (unless (file-exists-p (file-name-directory filename))
    (make-directory (file-name-directory filename)))
  ; take screenshot
  (if (eq system-type 'darwin)
      (call-process "screencapture" nil nil nil "-i" filename))
  (if (eq system-type 'gnu/linux)
      (call-process "import" nil nil nil filename))
  ; insert into file if correctly taken
  (if (file-exists-p filename)
    (insert (concat "[[file:" filename "]]")))  
  )

(use-package org
  :hook
  (
   ;; (org-mode . org-num-mode)
   (org-mode . (lambda ();; this will make sure auto-fill works for org-mode
                 (setq-local comment-auto-fill-only-comments nil)
                 (setq-local display-line-numbers-type 'absolute)
                 (setq-local org-use-speed-commands t))))
  :preface
  ;; Modules that should always be loaded together with org.el
  ;; `org-modules' default: (ol-w3m ol-bbdb ol-bibtex ol-docview ol-gnus ol-info
  ;;                         ol-irc ol-mhe ol-rmail ol-eww)
  (setq org-modules '(ol-info org-habit ol-gnus))

  ;; Set my default org-export backends. This variable need to be set before
  ;; org.el is loaded.
  (setq org-export-backends '(ascii html latex md odt))
  :custom
  (org-src-fontify-natively t) 
  (org-confirm-babel-evaluate nil)
  (org-clock-out-remove-zero-time-clocks t)
  (org-startup-folded 'content)
  (org-columns-default-format "%50ITEM(Task) %5TODO(Todo) %10Effort(Effort){:} %10CLOCKSUM(Clock) %2PRIORITY %TAGS")
  
  :config
  (setq org-hide-emphasis-markers t)
  ;; pomodoro implementation in org
  ;; https://github.com/lolownia/org-pomodoro
  (use-package org-pomodoro
    :after org-agenda
    :custom
    (org-pomodoro-ask-upon-killing t)
    (org-pomodoro-format "%s") ;;     
    (org-pomodoro-short-break-format "%s")
    (org-pomodoro-long-break-format  "%s")
    :custom-face
    (org-pomodoro-mode-line ((t (:foreground "#ff5555"))))
    (org-pomodoro-mode-line-break   ((t (:foreground "#50fa7b"))))
    :bind
    (:map org-agenda-mode-map
          ("p" . org-pomodoro)
    )
    )
  ;; (setq org-src-fontify-natively t)
  (setq org-ellipsis "⤵")
  (setq org-hide-leading-stars t)
  ;; org agenda files
  (setq org-agenda-files '("/home/espinosa/GoogleDrive/fractaliusfciencias/Org/agenda/inbox.org"
                           "/home/espinosa/GoogleDrive/fractaliusfciencias/Org/agenda/projects.org"))
  (setq org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-habit-show-habits t)
  (setq org-agenda-current-time-string "← now")
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((latex . t)
     (python . t))
   )
  ;; set defaults tags for org
  (setq org-tag-persistent-alist '(("@EMAIL" . ?e)
                                   ("@WRITE" . ?W)
                                   ("@CONFIGURE" . ?C)
                                   ("@WORK" . ?w)
                                   ("@PERSONAL" . ?l)
                                   ("@URGENT" . ?u)
                                   ("@LEARN" . ?n)
                                   ("@READ" . ?r)))

  (setq org-capture-templates
        '(("t" "Todo [inbox]" entry
          (file+headline "~/GoogleDrive/fractaliusfciencias/Org/agenda/inbox.org" "Tasks")
          (file "~/.emacs.d/etc/org-capture-templates/todo.txt"))
          ("b" "Add a book to read list" entry
           (file+headline "~/GoogleDrive/fractaliusfciencias/Org/agenda/books.org" "Read list")
           (file "~/.emacs.d/etc/org-capture-template/book.txt"))
          ("p" "Add a new project" entry
           (file+headline "~/GoogleDrive/fractaliusfciencias/Org/agenda/projects.org" "Projects")
           (file "~/.emacs.d/etc/org-capture-templates/projects.txt"))
          ("R" "Add a new reference" entry
           (file+headline "~/GoogleDrive/fractaliusfciencias/Org/agenda/references.org" "References")
           (file "~/.emacs.d/etc/org-capture-templates/reference.txt"))
          ("e" "Add a new code example" entry
           (file+headline "~/GoogleDrive/fractaliusfciencias/Org/agenda/examples.org" "Examples")
           (file "~/.emacs.d/etc/org-capture-templates/example.txt")) 
          )
        )

  ;; settings for org-refile
  (setq org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil)
  (setq org-refile-allow-creating-parent-nodes 'confirm)

  ;; Recalculate all org tables in the buffer when saving.
  ;; http://emacs.stackexchange.com/a/22221/115
  ;; Thu Jul 14 17:06:28 EDT 2016 - kmodi
  ;; Do not enable the buffer-wide recalculation by default because if an org
  ;; buffer has an org-table formula (like "#+TBLFM: $1=@#-1"), a *Calc*
  ;; buffer is created when `org-table-recalculate-buffer-tables' is run each
  ;; time.
  (defvar-local modi/org-table-enable-buffer-wide-recalculation nil
    "When non-nil, all the org tables in the buffer will be recalculated when
saving the file.
This variable is buffer local.")
  ;; Mark `modi/org-table-enable-buffer-wide-recalculation' as a safe local
  ;; variable as long as its value is t or nil. That way you are not prompted
  ;; to add that to `safe-local-variable-values' in custom.el.
  (put 'modi/org-table-enable-buffer-wide-recalculation 'safe-local-variable #'booleanp)  

  ;; Pretty bullets
  (use-package org-superstar
    :hook (org-mode . org-superstar-mode))
  )


;; A journaling tool with org-mode: `org-journal'
;; https://github.com/bastibe/org-journal
;; Quick summary:
;; To create a new journal entry: C-c C-j
;; To open today’s journal without creating a new entry: C-u C-c C-j
;; In calendar view:
;; * j to view an entry in a new buffer
;; * C-j to view an entry but not switch to it
;; * i j to add a new entry
;; * f w to search in all entries of the current week
;; * f m to search in all entries of the current month
;; * f y to search in all entries of the current year
;; * f f to search in all entries of all time
;; * [ to go to previous entry
;; * ] to go to next ;entr
;; When viewing a journal entry:
;; * C-c C-f to view next entry
;; * C-c C-b to view previous entry
(use-package org-journal
  :bind (("C-c o j" . org-journal-new-entry))
  :hook ((org-journal-mode . (lambda ()
                               (visual-line-mode -1))))
  :config
  (setq org-journal-dir "~/GoogleDrive/fractaliusfciencias/Org/Diary")
  )

(use-package hide-mode-line)

(defun efs/presentation-setup ()
  ;; Hide the mode line
  (hide-mode-line-mode 1)

  ;; Display images inline
  (org-display-inline-images) ;; Can also use org-startup-with-inline-images

  ;; Scale the text.  The next line is for basic scaling:
  (setq text-scale-mode-amount 3)
  (text-scale-mode 1))

  ;; This option is more advanced, allows you to scale other faces too
  ;; (setq-local face-remapping-alist '((default (:height 2.0) variable-pitch)
  ;;                                    (org-verbatim (:height 1.75) org-verbatim)
  ;;                                    (org-block (:height 1.25) org-block))))

(defun efs/presentation-end ()
  ;; Show the mode line again
  (hide-mode-line-mode 0)

  ;; Turn off text scale mode (or use the next line if you didn't use text-scale-mode)
  (text-scale-mode 0))

  ;; If you use face-remapping-alist, this clears the scaling:
  ;; (setq-local face-remapping-alist '((default variable-pitch default))))

(use-package org-tree-slide
  :hook
  ((org-tree-slide-play . efs/presentation-setup)
   (org-tree-slide-stop . efs/presentation-end))
  :custom
  (org-tree-slide-slide-in-effect t)
  (org-tree-slide-activate-message "Presentation started!")
  (org-tree-slide-deactivate-message "Presentation finished!")
  (org-tree-slide-header t)
  (org-tree-slide-breadcrumbs " > ")
  (org-image-actual-width nil)
  :bind
  ((:map org-tree-slide-mode-map
         ("C-h" . org-tree-slide-move-previous-tree)
         ("C-l" . org-tree-slide-move-next-tree)
    ))
  
  )


(provide 'SetupOrg)
