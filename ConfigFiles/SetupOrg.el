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

(use-package org
  :hook
  ((org-mode . org-num-mode)
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

  :config

  ;; org agenda files
  (setq org-agenda-files '("/home/espinosa/Google_Drive/fractaliusfciencias/Org/agenda/inbox.org"
                           "/home/espinosa/Google_Drive/fractaliusfciencias/Org/agenda/projects.org"))

  (setq org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-habit-show-habits t)

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
          (file+headline "~/Google_Drive/fractaliusfciencias/Org/agenda/inbox.org" "Tasks")
          (file "~/.emacs.d/etc/org-capture-templates/todo.txt"))
          ("b" "Add a book to read list" entry
           (file+headline "~/Google_Drive/fractaliusfciencias/Org/agenda/books.org" "Read list")
           (file "~/.emacs.d/etc/org-capture-template/book.txt"))
          ("p" "Add a new project" entry
           (file+headline "~/Google_Drive/fractaliusfciencias/Org/agenda/projects.org" "Projects")
           (file "~/.emacs.d/etc/org-capture-templates/projects.txt"))
          ("R" "Add a new reference" entry
           (file+headline "~/Google_Drive/fractaliusfciencias/Org/agenda/references.org" "References")
           (file "~/.emacs.d/etc/org-capture-templates/reference.txt"))
          ("e" "Add a new code example" entry
           (file+headline "~/Google_Drive/fractaliusfciencias/Org/agenda/examples.org" "Examples")
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
  (setq org-journal-dir "~/Google_Drive/fractaliusfciencias/Org/Diary")
  )


(provide 'SetupOrg)
