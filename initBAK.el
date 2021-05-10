;; Emacs Configuration

;; Add GNU, Melpa and Org package to repos
(eval-when-compile
  (require 'package)
  (package-initialize)
;;  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
  (add-to-list 'package-archives '("gnu" . "http://mirrors.163.com/elpa/gnu/") t)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package)
    (package-install 'diminish)
    (package-install 'quelpa)
    (package-install 'bind-key))

  (setq use-package-always-ensure t)
  (setq use-package-expand-minimally t)

  (require 'use-package))
(require 'diminish)
(require 'bind-key)
;; Server
(use-package server
  :ensure nil
  :hook (after-init . server-mode))

;; Ignore split window horizontally
(setq split-width-threshold nil)
(setq split-width-threshold 160)

;; Quiet Startup
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)
(setq initial-scratch-message nil)

(defun display-startup-echo-area-message ()
  (message ""))

;; Default Encoding
(prefer-coding-system 'utf-8-unix)
(set-locale-environment "en_US.UTF-8")
(set-default-coding-systems 'utf-8-unix)
(set-selection-coding-system 'utf-8-unix)
(set-buffer-file-coding-system 'utf-8-unix)
(set-clipboard-coding-system 'utf-8) ; included by set-selection-coding-system
(set-keyboard-coding-system 'utf-8) ; configured by prefer-coding-system
(set-terminal-coding-system 'utf-8) ; configured by prefer-coding-system
(setq buffer-file-coding-system 'utf-8) ; utf-8-unix
(setq save-buffer-coding-system 'utf-8-unix) ; nil
(setq process-coding-system-alist
  (cons '("grep" utf-8 . utf-8) process-coding-system-alist))

;; Misc
;; (setq frame-title-format nil)
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

;; Alias yes or no
(defalias 'yes-or-no-p #'y-or-n-p)

;; GUI/Font
(if window-system
    (progn
      ;; UI parts
      (toggle-scroll-bar 0)
      (tool-bar-mode 0)
      ;; (menu-bar-mode 0)

      ;; Overwrite latin and greek char's font
      (defun set-latin-and-greek-font (family)
        (set-fontset-font (frame-parameter nil 'font) '(#x0250 . #x02AF) (font-spec :family family)) ; IPA extensions
        (set-fontset-font (frame-parameter nil 'font) '(#x00A0 . #x00FF) (font-spec :family family)) ; latin-1
        (set-fontset-font (frame-parameter nil 'font) '(#x0100 . #x017F) (font-spec :family family)) ; latin extended-A
        (set-fontset-font (frame-parameter nil 'font) '(#x0180 . #x024F) (font-spec :family family)) ; latin extended-B
        (set-fontset-font (frame-parameter nil 'font) '(#x2018 . #x2019) (font-spec :family family)) ; end quote
        (set-fontset-font (frame-parameter nil 'font) '(#x2588 . #x2588) (font-spec :family family)) ; █
        (set-fontset-font (frame-parameter nil 'font) '(#x2500 . #x2500) (font-spec :family family)) ; ─
        (set-fontset-font (frame-parameter nil 'font) '(#x2504 . #x257F) (font-spec :family family)) ; box character
        (set-fontset-font (frame-parameter nil 'font) '(#x0370 . #x03FF) (font-spec :family family)))

      (setq use-default-font-for-symbols nil)
      (setq inhibit-compacting-font-caches t)
      (setq default-font-family "FuraCode Nerd Font")

      (set-face-attribute 'default nil :family default-font-family :height 120)
      (set-latin-and-greek-font default-font-family)
      ;; (add-to-list 'face-font-rescale-alist (cons default-font-family 0.86))
      ))

;; Delete selection if insert someting
(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))

;; Automatically reload files was modified by external program
(use-package autorevert
  :ensure nil
  :diminish
  :hook (after-init . global-auto-revert-mode))

;; Hungry deletion
(use-package hungry-delete
  :diminish
  :hook (after-init . global-hungry-delete-mode)
  :config (setq-default hungry-delete-chars-to-skip " \t\f\v"))

;; SmartParens
(use-package smartparens
  :hook
  (after-init . smartparens-global-mode)
  :config
  (require 'smartparens-config)
  (sp-pair "=" "=" :actions '(wrap))
  (sp-pair "+" "+" :actions '(wrap))
  (sp-pair "<" ">" :actions '(wrap))
  (sp-pair "$" "$" :actions '(wrap)))

;; History
(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode))

;; Recent files
(use-package recentf
  :ensure nil
  :hook (after-init . recentf-mode)
  :custom
  (recentf-max-saved-items 20000000)
  (recentf-auto-cleanup 'never)
  (recentf-exclude '((expand-file-name package-user-dir)
                     ".cache"
                     "cache"
                     "recentf"
                     "COMMIT_EDITMSG\\'"))
  :preface
  (defun espinosa/recentf-save-list-silence ()
    (interactive)
    (let ((message-log-max nil))
      (if (fboundp 'shut-up)
          (shut-up (recentf-save-list))
        (recentf-save-list)))
    (message ""))
  (defun espinosa/recentf-cleanup-silence ()
    (interactive)
    (let ((message-log-max nil))
      (if shutup-p
          (shut-up (recentf-cleanup))
        (recentf-cleanup)))
    (message ""))
  :hook
  (focus-out-hook . (espinosa/recentf-save-list-silence espinosa/recentf-cleanup-silence))
)

;; common
(use-package exec-path-from-shell
  :custom
  (exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-variables '("PATH"))
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))
)

;; xclip
(use-package xclip
  :if (eq system-type 'gnu/linux)
  :config
  (xclip-mode 1))

;; Icons
(use-package all-the-icons
  :defer t)

;; Posframe (pop windows)
(use-package posframe)

;; point
(use-package popwin)
(use-package point-history
  :load-path "/home/espinosa/Google_Drive/fractaliusfciencias/Emacs/src/github.com/blue0513/point-history"
  :config
  (point-history-mode t))

;; Mouse
(xterm-mouse-mode t)
(global-set-key   [mouse-4] '(lambda () (interactive) (scroll-down 1)))
(global-set-key   [mouse-5] '(lambda () (interactive) (scroll-up   1)))

;; Custom Maps
(define-prefix-command 'espinosa-window-map)
(define-key global-map (kbd "M-o") 'espinosa-window-map)

(define-prefix-command 'espinosa-toggle-map)
(define-key global-map (kbd "M-t") 'espinosa-toggle-map)

(define-prefix-command 'espinosa-link-map)
(define-key global-map (kbd "M-o l") 'espinosa-link-map)

(define-prefix-command 'espinosa-customs)
(define-key global-map (kbd "C-e ") 'espinosa-customs) 


;; Global
(global-unset-key "\C-z")
(global-set-key (kbd "M-o m")   'async-shell-command)
(global-set-key (kbd "M-o b")   'switch-to-buffer)
(global-set-key (kbd "C-x C-b") 'switch-to-buffer)
(global-set-key (kbd "C-M-g")   'goto-line)
(global-set-key (kbd "C-h")     'backward-delete-char)
(global-set-key (kbd "C-o")     'other-window)
(global-set-key (kbd "M-:")     'comment-dwim)
(global-set-key (kbd "C-m")     'electric-newline-and-maybe-indent)
(global-set-key (kbd "C-j")     'newline-and-indent)
(global-set-key (kbd "M-r")     'rename-file)
(global-set-key (kbd "M-t l")   'toggle-truncate-lines)

;; Which key
(use-package which-key
  :diminish which-key-mode
  :hook (after-init . which-key-mode))

;; hydra
(use-package hydra
  :config
  (use-package hydra-posframe
    :load-path "/home/espinosa/Google_Drive/fractaliusfciencias/Emacs/src/github.com/Ladicle/hydra-posframe"
    :custom
    (hydra-posframe-parameters
      '((left-fringe . 5)
        (right-fringe . 5)))
    :custom-face
    (hydra-posframe-border-face ((t (:background "#6272a4"))))
    :hook (after-init . hydra-posframe-enable)))

;; Undo Redo
(use-package undo-tree
  :bind
  ("M-/" . undo-tree-redo)
  :config
  (global-undo-tree-mode))

;; projectile
(use-package projectile
  :diminish
  :bind
  ("M-o p" . counsel-projectile-switch-project)
  :config
  (projectile-mode +1))

;; Wgrep
(use-package wgrep
  :defer t
  :custom
  (wgrep-enable-key "e")
  (wgrep-auto-save-buffer t)
  (wgrep-change-readonly-file t))

;; Ag
(use-package ag
  :custom
  (ag-highligh-search t)
  (ag-reuse-buffers t)
  (ag-reuse-window t)
  :bind
  ("M-s a" . ag-project)
  :config
  (use-package wgrep-ag))

;; Ivy
  (use-package counsel
    :diminish ivy-mode counsel-mode
    :defines
    (projectile-completion-system magit-completing-read-function)
    :bind
    (("C-s" . swiper)
    ("M-s r" . ivy-resume)
    ("C-c v p" . ivy-push-view)
    ("C-c v o" . ivy-pop-view)
    ("C-c v ." . ivy-switch-view)
    ("M-s c" . counsel-ag)
    ("M-o f" . counsel-fzf)
    ("M-o r" . counsel-recentf)
    ("M-y" . counsel-yank-pop)
    :map ivy-minibuffer-map
    ("C-w" . ivy-backward-kill-word)
    ("C-k" . ivy-kill-line)
    ("C-j" . ivy-immediate-done)
    ("RET" . ivy-alt-done)           
    ("C-h" . ivy-backward-delete-char))
    :preface
    (defun ivy-format-function-pretty (cands)
      "Transform CANDS into a string for minibuffer."
      (ivy--format-function-generic
       (lambda (str)
         (concat
             (all-the-icons-faicon "hand-o-right" :height .85 :v-adjust .05 :face 'font-lock-constant-face)
             (ivy--add-face str 'ivy-current-match)))
       (lambda (str)
         (concat "  " str))
       cands
       "\n"))
    :hook
    (after-init . ivy-mode)
    (ivy-mode . counsel-mode)
    :custom
    (enable-recursive-minibuffers t)
    (ivy-use-selectable-prompt t)
    (ivy-use-virtual-buffers t)
    (ivy-on-del-error-function nil)
    (swiper-action-recenter t)
    (counsel-grep-base-command "ag -S --noheading --nocolor --nofilename --numbers '%s' %s")
    :config
    (setq counsel-yank-pop-height 15)
    ;; using ivy-format-fuction-arrow with counsel-yank-pop
    (advice-add
    'counsel--yank-pop-format-function
    :override
    (lambda (cand-pairs)
      (ivy--format-function-generic
	(lambda (str)
	  (mapconcat
	    (lambda (s)
	      (ivy--add-face (concat (propertize "| " 'face `(:foreground "#61bfff")) s) 'ivy-current-match))
	    (split-string 
	      (counsel--yank-pop-truncate str) "\n" t)
	    "\n"))
	(lambda (str)
	  (counsel--yank-pop-truncate str))
	cand-pairs
	counsel-yank-pop-separator))
    )

    ;; NOTE: this variable do not work if defined in :custom
    (setq ivy-format-function 'ivy-format-function-pretty)
    (setq counsel-yank-pop-separator
        (propertize "\n────────────────────────────────────────────────────────\n"
               'face `(:foreground "#6272a4")))

    ;; Integration with `projectile'
    (with-eval-after-load 'projectile
      (setq projectile-completion-system 'ivy))
    ;; Integration with `magit'
    (with-eval-after-load 'magit
      (setq magit-completing-read-function 'ivy-completing-read))
    ;; Enhance fuzzy matching
    (use-package flx)
    ;; Enhance M-x
    (use-package amx)
    ;; Ivy integration for Projectile
    (use-package counsel-projectile
      :config (counsel-projectile-mode 1))
  ;; Show ivy frame using posframe
  (use-package ivy-posframe
    :custom
    (ivy-display-function #'ivy-posframe-display-at-frame-center)
    ;; (ivy-posframe-width 130)
    ;; (ivy-posframe-height 11)
    (ivy-posframe-parameters
      '((left-fringe . 5)
        (right-fringe . 5)))
    :custom-face
    (ivy-posframe ((t (:background "#282a36"))))
    (ivy-posframe-border ((t (:background "#6272a4"))))
    (ivy-posframe-cursor ((t (:background "#61bfff"))))
    :hook
    (ivy-mode . ivy-posframe-enable))

  ;; ghq
  (use-package ivy-ghq
    :load-path "/home/espinosa/Google_Drive/fractaliusfciencias/Emacs/src/github.com/analyticd/ivy-ghq"
    :commands (ivy-ghq-open)
    :bind
    ("M-o p" . ivy-ghq-open-and-fzf)
    :custom
    (ivy-ghq-short-list t)
    :preface
    (defun ivy-ghq-open-and-fzf ()
      (interactive)
      (ivy-ghq-open)
      (counsel-fzf)))
  ;; More friendly display transformer for Ivy
  (use-package ivy-rich
    :defines (all-the-icons-dir-icon-alist bookmark-alist)
    :functions (all-the-icons-icon-family
                all-the-icons-match-to-alist
                all-the-icons-auto-mode-match?
                all-the-icons-octicon
                all-the-icons-dir-is-submodule)
    :preface
    (defun ivy-rich-bookmark-name (candidate)
      (car (assoc candidate bookmark-alist)))

    (defun ivy-rich-repo-icon (candidate)
      "Display repo icons in `ivy-rich`."
      (all-the-icons-octicon "repo" :height .9))

    (defun ivy-rich-org-capture-icon (candidate)
      "Display repo icons in `ivy-rich`."
      (pcase (car (last (split-string (car (split-string candidate)) "-")))
         ("emacs" (all-the-icons-fileicon "emacs" :height .68 :v-adjust .001))
         ("schedule" (all-the-icons-faicon "calendar" :height .68 :v-adjust .005))
         ("tweet" (all-the-icons-faicon "commenting" :height .7 :v-adjust .01))
         ("link" (all-the-icons-faicon "link" :height .68 :v-adjust .01))
         ("memo" (all-the-icons-faicon "pencil" :height .7 :v-adjust .01))
         (_       (all-the-icons-octicon "inbox" :height .68 :v-adjust .01))
         ))

    (defun ivy-rich-org-capture-title (candidate)
      (let* ((octl (split-string candidate))
             (title (pop octl))
             (desc (mapconcat 'identity octl " ")))
        (format "%-25s %s"
                 title
                 (propertize desc 'face `(:inherit font-lock-doc-face)))))

    (defun ivy-rich-buffer-icon (candidate)
      "Display buffer icons in `ivy-rich'."
      (when (display-graphic-p)
        (when-let* ((buffer (get-buffer candidate))
                    (major-mode (buffer-local-value 'major-mode buffer))
                    (icon (if (and (buffer-file-name buffer)
                                   (all-the-icons-auto-mode-match? candidate))
                              (all-the-icons-icon-for-file candidate)
                            (all-the-icons-icon-for-mode major-mode))))
          (if (symbolp icon)
              (setq icon (all-the-icons-icon-for-mode 'fundamental-mode)))
          (unless (symbolp icon)
            (propertize icon
                        'face `(
                                :height 1.1
                                :family ,(all-the-icons-icon-family icon)
                                ))))))

    (defun ivy-rich-file-icon (candidate)
      "Display file icons in `ivy-rich'."
      (when (display-graphic-p)
        (let ((icon (if (file-directory-p candidate)
                        (cond
                         ((and (fboundp 'tramp-tramp-file-p)
                               (tramp-tramp-file-p default-directory))
                          (all-the-icons-octicon "file-directory"))
                         ((file-symlink-p candidate)
                          (all-the-icons-octicon "file-symlink-directory"))
                         ((all-the-icons-dir-is-submodule candidate)
                          (all-the-icons-octicon "file-submodule"))
                         ((file-exists-p (format "%s/.git" candidate))
                          (all-the-icons-octicon "repo"))
                         (t (let ((matcher (all-the-icons-match-to-alist candidate all-the-icons-dir-icon-alist)))
                              (apply (car matcher) (list (cadr matcher))))))
                      (all-the-icons-icon-for-file candidate))))
          (unless (symbolp icon)
            (propertize icon
                        'face `(
                                :height 1.1
                                :family ,(all-the-icons-icon-family icon)
                                ))))))

    :hook (ivy-rich-mode . (lambda ()
                             (setq ivy-virtual-abbreviate
                                   (or (and ivy-rich-mode 'abbreviate) 'name))))
    :init
    (setq ivy-rich-display-transformers-list
          '(ivy-switch-buffer
            (:columns
             ((ivy-rich-buffer-icon)
              (ivy-rich-candidate (:width 30))
              (ivy-rich-switch-buffer-size (:width 7))
              (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
              (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
              (ivy-rich-switch-buffer-project (:width 15 :face success))
              (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
             :predicate
             (lambda (cand) (get-buffer cand)))
            ivy-switch-buffer-other-window
            (:columns
             ((ivy-rich-buffer-icon)
              (ivy-rich-candidate (:width 30))
              (ivy-rich-switch-buffer-size (:width 7))
              (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
              (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
              (ivy-rich-switch-buffer-project (:width 15 :face success))
              (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
             :predicate
             (lambda (cand) (get-buffer cand)))
            counsel-M-x
            (:columns
             ((counsel-M-x-transformer (:width 40))
              (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))
            counsel-describe-function
            (:columns
             ((counsel-describe-function-transformer (:width 45))
              (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))
            counsel-describe-variable
            (:columns
             ((counsel-describe-variable-transformer (:width 45))
              (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face))))
            counsel-find-file
            (:columns
             ((ivy-rich-file-icon)
              (ivy-rich-candidate)))
            counsel-file-jump
            (:columns
             ((ivy-rich-file-icon)
              (ivy-rich-candidate)))
            counsel-dired-jump
            (:columns
             ((ivy-rich-file-icon)
              (ivy-rich-candidate)))
            counsel-git
            (:columns
             ((ivy-rich-file-icon)
              (ivy-rich-candidate)))
            counsel-recentf
            (:columns
             ((ivy-rich-file-icon)
              (ivy-rich-candidate (:width 110))))
            counsel-bookmark
            (:columns
             ((ivy-rich-bookmark-type)
              (ivy-rich-bookmark-name (:width 30))
              (ivy-rich-bookmark-info (:width 80))))
            counsel-projectile-switch-project
            (:columns
             ((ivy-rich-file-icon)
              (ivy-rich-candidate)))
            counsel-fzf
            (:columns
             ((ivy-rich-file-icon)
              (ivy-rich-candidate)))
            ivy-ghq-open
            (:columns
             ((ivy-rich-repo-icon)
              (ivy-rich-candidate)))
            ivy-ghq-open-and-fzf
            (:columns
             ((ivy-rich-repo-icon)
              (ivy-rich-candidate)))
            counsel-projectile-find-file
            (:columns
             ((ivy-rich-file-icon)
              (ivy-rich-candidate)))
            counsel-org-capture
            (:columns
             ((ivy-rich-org-capture-icon)
              (ivy-rich-org-capture-title)
              ))
            counsel-projectile-find-dir
            (:columns
             ((ivy-rich-file-icon)
              (counsel-projectile-find-dir-transformer)))))

    (setq ivy-rich-parse-remote-buffer nil)
    :config
    (ivy-rich-mode 1))
)

;; Anzu
(use-package anzu
  :diminish
  :bind
  ("C-r"   . anzu-query-replace-regexp)
  ("C-M-r" . anzu-query-replace-at-cursor-thing)
  :hook
  (after-init . global-anzu-mode)
)

;; Multiple Cursor
(use-package multiple-cursors
  :functions hydra-multiple-cursors
  :bind
  ("M-u" . hydra-multiple-cursors/body)
  :preface
  ;; insert specific serial number
  (defvar espinosa/mc/insert-numbers-hist nil)
  (defvar espinosa/mc/insert-numbers-inc 1)
  (defvar espinosa/mc/insert-numbers-pad "%01d")

  (defun espinosa/mc/insert-numbers (start inc pad)
    "Insert increasing numbers for each cursor specifically."
    (interactive
     (list (read-number "Start from: " 0)
           (read-number "Increment by: " 1)
           (read-string "Padding (%01d): " nil espinosa/mc/insert-numbers-hist "%01d")))
    (setq mc--insert-numbers-number start)
    (setq espinosa/mc/insert-numbers-inc inc)
    (setq espinosa/mc/insert-numbers-pad pad)
    (mc/for-each-cursor-ordered
     (mc/execute-command-for-fake-cursor
      'espinosa/mc--insert-number-and-increase
      cursor)))

  (defun espinosa/mc--insert-number-and-increase ()
    (interactive)
    (insert (format espinosa/mc/insert-numbers-pad mc--insert-numbers-number))
    (setq mc--insert-numbers-number (+ mc--insert-numbers-number espinosa/mc/insert-numbers-inc)))

  :config
  (with-eval-after-load 'hydra
    (defhydra hydra-multiple-cursors (:color pink :hint nil)
"
                                                                        ╔════════╗
    Point^^^^^^             Misc^^            Insert                            ║ Cursor ║
  ──────────────────────────────────────────────────────────────────────╨────────╜
     _k_    _K_    _M-k_    [_l_] edit lines  [_i_] 0...
     ^↑^    ^↑^     ^↑^     [_m_] mark all    [_a_] letters
    mark^^ skip^^^ un-mk^   [_s_] sort        [_n_] numbers
     ^↓^    ^↓^     ^↓^
     _j_    _J_    _M-j_
  ╭──────────────────────────────────────────────────────────────────────────────╯
                           [_q_]: quit, [Click]: point
"
          ("l" mc/edit-lines :exit t)
          ("m" mc/mark-all-like-this :exit t)
          ("j" mc/mark-next-like-this)
          ("J" mc/skip-to-next-like-this)
          ("M-j" mc/unmark-next-like-this)
          ("k" mc/mark-previous-like-this)
          ("K" mc/skip-to-previous-like-this)
          ("M-k" mc/unmark-previous-like-this)
          ("s" mc/mark-all-in-region-regexp :exit t)
          ("i" mc/insert-numbers :exit t)
          ("a" mc/insert-letters :exit t)
          ("n" espinosa/mc/insert-numbers :exit t)
          ("<mouse-1>" mc/add-cursor-on-click)
          ;; Help with click recognition in this hydra
          ("<down-mouse-1>" ignore)
          ("<drag-mouse-1>" ignore)
          ("q" nil)))
)

;; Avy/ace
(use-package avy
    :functions (hydra-avy hydra-viewer)
    :bind
    ("C-'"   . avy-resume)
    ("C-:"   . avy-goto-char-2-below)
    ("C-;"   . avy-goto-char)
    ("M-j"   . hydra-avy/body)
    ("C-M-v" . hydra-viewer/body)
    :preface
    ;; fixed cursor scroll-up
    (defun scroll-up-in-place (n)
      (interactive "p")
      (forward-line (- n))
      (scroll-down n))
    ;; fixed cursor scroll-down
    (defun scroll-down-in-place (n)
      (interactive "p")
      (forward-line n)
      (scroll-up n))
    ;; yank inner sexp
    (defun yank-inner-sexp ()
      (interactive)
      (backward-list)
      (mark-sexp)
      (copy-region-as-kill (region-beginning) (region-end))
    )
    :config
    (when (eq system-type 'darwin)
      (progn
        (global-set-key (kbd "C-:") 'avy-goto-char)
        (global-set-key (kbd "C-;") 'avy-goto-char-2-below)))

    (use-package avy-zap
      :bind
      ("M-z" . avy-zap-to-char-dwim)
      ("M-z" . avy-zap-up-to-char-dwim)
      )

    (with-eval-after-load 'hydra
        (defhydra hydra-viewer (:color pink :hint nil)
          "
                                                                        ╔════════╗
   Char/Line^^^^^^  Word/Page^^^^^^^^  Line/Buff^^^^   Paren                              ║ Window ║
  ──────────────────────────────────────────────────────────────────────╨────────╜
       ^^_k_^^          ^^_u_^^          ^^_g_^^       _(_ ← _y_ → _)_
       ^^^↑^^^          ^^^↑^^^          ^^^↑^^^       _,_ ← _/_ → _._
   _h_ ← _d_ → _l_  _H_ ← _D_ → _L_  _a_ ← _K_ → _e_
       ^^^↓^^^          ^^^↓^^^          ^^^↓^
       ^^_j_^^          ^^_n_^^          ^^_G_
  ╭──────────────────────────────────────────────────────────────────────────────╯
                           [_q_]: quit, [_<SPC>_]: center
          "
          ("j" scroll-down-in-place)
          ("k" scroll-up-in-place)
          ("l" forward-char)
          ("d" delete-char)
          ("h" backward-char)
          ("L" forward-word)
          ("H" backward-word)
          ("u" scroll-up-command)
          ("n" scroll-down-command)
          ("D" delete-word-at-point)
          ("a" mwim-beginning-of-code-or-line)
          ("e" mwim-end-of-code-or-line)
          ("g" beginning-of-buffer)
          ("G" end-of-buffer)
          ("K" kill-whole-line)
          ("(" backward-list)
          (")" forward-list)
          ("y" yank-inner-sexp)
          ("." backward-forward-next-location)
          ("," backward-forward-previous-location)
          ("/" avy-goto-char :exit t)
          ("<SPC>" recenter-top-bottom)
          ("q" nil))

        (defhydra hydra-avy (:color pink :hint nil)
          "
                                                                        ╔════════╗
        ^^Goto^^        Kill^^        Yank^^        Move^^        Misc            ║  Jump  ║
  ──────────────────────────────────────────────────────────────────────╨────────╜
    _c_ ← char^^        [_k_] region  [_y_] region  [_m_] region  [_n_] line number
    _a_ ← char2 → _b_   [_K_] line    [_Y_] line    [_M_] line    [_v_] Goto viewer
    _w_ ← word  → _W_   [_z_] zap^^^^                             [_o_] Goto clock
    _l_ ← line  → _e_   ^^^^^                                     _,_ ← f!y → _._
  ╭──────────────────────────────────────────────────────────────────────────────╯
                      [_q_]: quit, [_i_]: imenu, [_<SPC>_]: resume
"
          ("c" avy-goto-char :exit t)
          ("a" avy-goto-char-2 :exit t)
          ("b" avy-goto-char-below :exit t)
          ("w" avy-goto-word-1 :exit t)
          ("W" avy-goto-word-1-below :exit t)
          ("l" avy-goto-line :exit t)
          ("e" avy-goto-end-of-line :exit t)
          ("M" avy-move-line)
          ("m" avy-move-region)
          ("K" avy-kill-whole-line)
          ("k" avy-kill-region)
          ("Y" avy-copy-line :exit t)
          ("y" avy-copy-region :exit t)
          ("n" goto-line :exit t)
          ("o" org-clock-jump-to-current-clock :exit t)
          ("z" avy-zap-to-char-dwim :exit t)
          ("v" hydra-viewer/body :exit t)
          ("<SPC>" avy-resume :exit t)
          ("o" org-clock-jump-to-current-clock :exit t)
          ("i" counsel-imenu :exit t)
          ("," flymake-goto-previous-error)
          ("." flymake-goto-next-error)
          ("q" nil)))
)

(use-package ace-window
    :functions hydra-frame-window/body
    :bind
    ("C-M-o" . hydra-frame-window/body)
    ("M-t m" . espinosa/toggle-window-maximize)
    :custom
    (aw-keys '(?j ?k ?l ?i ?o ?h ?y ?u ?p))
    :custom-face
    (aw-leading-char-face ((t (:height 4.0 :foreground "#f1fa8c"))))
    :preface
    (defvar is-window-maximized nil)
    (defun espinosa/toggle-window-maximize ()
        (interactive)
        (progn
          (if is-window-maximized
              (balance-windows)
            (maximize-window))
          (setq is-window-maximized
                (not is-window-maximized))))
    (defun hydra-title(title) (propertize title 'face `(:inherit font-lock-warning-face :weight bold)))
    (defun command-name(title) (propertize title 'face `(:foreground "#f8f8f2")))
    (defun spacer() (propertize "." 'face `(:foreground "#282a36")))
    :config
    (use-package rotate
        :load-path "/home/espinosa/Google_Drive/fractaliusfciencias/Emacs/src/github.com/daichirata/emacs-rotate"
        :bind
        ("M-o SPC" . rotate-layout))
    (with-eval-after-load 'hydra
        (defhydra hydra-frame-window (:color blue :hint nil)
        (format
         (format "%s" (propertize "                                                                       ╔════════╗
    ((%s))^^^^^^^^   ((%s))^^^^  ((%s))^^  ((%s))^^  ((%s))^^^^^^  ((%s))^   ║ Window ║
^^^^^^ ──────────────────────────────────────────────────────────────────────╨────────╜
        ^_k_^        %s_+_         _-_       %s     _,_ ← %s → _._^  %s
        ^^↑^^          ^↑^         ^↑^       %s
    _h_ ←   → _l_   ^^%s%s^^^^^    ^%s    ^^^%s^^^^     %s
        ^^↓^^          ^↓^         ^↓^       %s^^       %s
        ^_j_^        %s_=_         _/_       %s
^^^^^^ ┌──────────────────────────────────────────────────────────────────────────────┘
                           [_q_]: %s, [_<SPC>_]: %s" 'face `(:inherit font-lock-doc-face)))
                           (hydra-title "Size")
                           (hydra-title "Zoom")
                           (hydra-title "Split")
                           (hydra-title "Window")
                           (hydra-title "Buffer")
                           (hydra-title "Misc")
                           (all-the-icons-material "zoom_in" :height .85 :face 'font-lock-doc-face)
                           (command-name "_o_ther")
                           (command-name "page")
                           (command-name "_r_centf")
                           (command-name "_s_wap")
                           (all-the-icons-faicon "slideshare" :height .85 :face 'font-lock-doc-face)
                           (command-name "_p_mode")
                           (command-name "w_i_ndow")
                           (command-name "_m_aximize")
                           (command-name "_s_witch")
                           (command-name "_d_elete")
                           (command-name "_D_elete")
                           (all-the-icons-material "zoom_out" :height .85 :face 'font-lock-doc-face)
                           (command-name "del_O_thers")
                           (command-name "quit")
                           (command-name "rotate")
                           )

          ("K" kill-current-buffer :exit t)
          ("D" kill-buffer-and-window :exit t)
          ("O" delete-other-windows  :exit t)
          ("F" toggle-frame-fullscreen)
          ("i" ace-window)
          ("s" ace-swap-window :exit t)
          ("d" ace-delete-window)
          ("m" espinosa/toggle-window-maximize :exit t)
          ("=" text-scale-decrease)
          ("+" text-scale-increase)
          ("-" split-window-vertically)
          ("/" split-window-horizontally)
          ("h" shrink-window-horizontally)
          ("k" shrink-window)
          ("j" enlarge-window)
          ("l" enlarge-window-horizontally)
          ("," previous-buffer)
          ("." next-buffer)
          ("o" other-window)
          ("p" presentation-mode)
          ("r" counsel-recentf :exit t)
          ("s" switch-to-buffer :exit t)
          ("D" kill-buffer-and-window)
          ("<SPC>" rotate-layout)
          ("q" nil)))
)

;; Smart-move
(use-package mwim
  :bind
  ("C-a" . mwim-beginning-of-code-or-line)
  ("C-e" . mwim-end-of-code-or-line))

;; flymake
(use-package flymake-posframe
  :load-path "/home/espinosa/Google_Drive/fractaliusfciencias/Emacs/src/github.com/Ladicle/flymake-posframe"
  :custom
  (flymake-posframe-error-prefix " ")
  :custom-face
  (flymake-posframe-foreground-face ((t (:foreground "white"))))
  :hook (flymake-mode . flymake-posframe-mode)
)

(use-package flymake-diagnostic-at-point
  :disabled
  :after flymake
  :custom
  (flymake-diagnostic-at-point-timer-delay 0.1)
  (flymake-diagnostic-at-point-error-prefix " ")
  (flymake-diagnostic-at-point-display-diagnostic-function 'flymake-diagnostic-at-point-display-popup) ;; or flymake-diagnostic-at-point-display-minibuffer
  :hook
  (flymake-mode . flymake-diagnostic-at-point-mode)
)

;; flyspell
(use-package flyspell
  :diminish
  :if (executable-find "aspell")
  :hook
  ((org-mode yaml-mode markdown-mode git-commit-mode) . flyspell-mode)
  (prog-mode . flyspell-prog-mode)
  (before-save-hook . flyspell-buffer)
  (flyspell-mode . (lambda ()
                     (dolist (key '("C-;" "C-," "C-."))
                       (unbind-key key flyspell-mode-map))))
  :custom
  (flyspell-issue-message-flag nil)
  (ispell-program-name "aspell")
  (ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together"))
  :custom-face
  (flyspell-incorrect ((t (:underline (:color "#f1fa8c" :style wave)))))
  (flyspell-duplicate ((t (:underline (:color "#50fa7b" :style wave)))))
  :preface
  (defun message-off-advice (oldfun &rest args)
    "Quiet down messages in adviced OLDFUN."
    (let ((message-off (make-symbol "message-off")))
      (unwind-protect
          (progn
            (advice-add #'message :around #'ignore (list 'name message-off))
            (apply oldfun args))
        (advice-remove #'message message-off))))
  :config
  (advice-add #'ispell-init-process :around #'message-off-advice)
  (use-package flyspell-correct-ivy
    :bind ("C-M-:" . flyspell-correct-at-point)
    :config
    (when (eq system-type 'darwin)
      (progn
        (global-set-key (kbd "C-M-;") 'flyspell-correct-at-point)))
    (setq flyspell-correct-interface #'flyspell-correct-ivy))
)

;; Persp
(use-package persp-mode
  :disabled
  :diminish
  :defines ivy-sort-functions-alist
  :commands (get-current-persp persp-contain-buffer-p persp-add persp-by-name-and-exists)
  :hook ((after-init . persp-mode)
         (emacs-startup . toggle-frame-maximized))
  :custom
  (persp-keymap-prefix (kbd "C-x p"))
  (persp-nil-name "default")
  (persp-set-last-persp-for-new-frames nil)
  (persp-auto-resume-time 0)
  :config
  ;; NOTE: Redefine `persp-add-new' to address.
  ;; Issue: Unable to create/handle persp-mode
  ;; https://github.com/Bad-ptr/persp-mode.el/issues/96
  ;; https://github.com/Bad-ptr/persp-mode-projectile-bridge.el/issues/4
  ;; https://emacs-china.org/t/topic/6416/7
  (defun* persp-add-new (name &optional (phash *persp-hash*))
    "Create a new perspective with the given `NAME'. Add it to `PHASH'.
  Return the created perspective."
    (interactive "sA name for the new perspective: ")
    (if (and name (not (equal "" name)))
        (destructuring-bind (e . p)
            (persp-by-name-and-exists name phash)
          (if e p
            (setq p (if (equal persp-nil-name name)
                        nil (make-persp :name name)))
            (persp-add p phash)
            (run-hook-with-args 'persp-created-functions p phash)
            p))
      (message "[persp-mode] Error: Can't create a perspective with empty name.")
      nil))

  ;; Ignore temporary buffers
  (add-hook 'persp-common-buffer-filter-functions
            (lambda (b) (or (string-prefix-p "*" (buffer-name b))
                       (string-prefix-p "magit" (buffer-name b)))))

  ;; Integrate IVY
  (with-eval-after-load "ivy"
    (add-hook 'ivy-ignore-buffers
              #'(lambda (b)
                  (when persp-mode
                    (let ((persp (get-current-persp)))
                      (if persp
                          (not (persp-contain-buffer-p b persp))
                        nil)))))

    (setq ivy-sort-functions-alist
          (append ivy-sort-functions-alist
                  '((persp-kill-buffer   . nil)
                    (persp-remove-buffer . nil)
                    (persp-add-buffer    . nil)
                    (persp-switch        . nil)
                    (persp-window-switch . nil)
                    (persp-frame-switch  . nil)))))
)

;; yasnippet
(use-package yasnippet
  :diminish yas-minor-mode
  :custom (yas-snippet-dirs '("~/.emacs.d/snippets"))
  :hook (after-init . yas-global-mode)
)

;; Company
(use-package company
  :diminish company-mode
  :defines
  (company-dabbrev-ignore-case company-dabbrev-downcase)
  :bind
  (:map company-active-map
   ("C-n" . company-select-next)
   ("C-p" . company-select-previous)
   ("<tab>" . company-complete-common-or-cycle)
   :map company-search-map
   ("C-p" . company-select-previous)
   ("C-n" . company-select-next))
  :custom
  (company-idle-delay 0)
  (company-echo-delay 0)
  (company-minimum-prefix-length 1)
  :hook
  (after-init . global-company-mode)
  (plantuml-mode . (lambda () (set (make-local-variable 'company-backends)
                            '((company-yasnippet
                               ;; company-dabbrev
                               )))))
  ((go-mode
    c++-mode
    c-mode
    objc-mode) . (lambda () (set (make-local-variable 'company-backends)
                            '((company-yasnippet
                               company-lsp
                               company-files
                               ;; company-dabbrev-code
                               )))))
  :config
  ;; using child frame
  (use-package company-posframe
    :hook (company-mode . company-posframe-mode))
  ;; Show pretty icons
  (use-package company-box
    :diminish
    :hook (company-mode . company-box-mode)
    :init (setq company-box-icons-alist 'company-box-icons-all-the-icons)
    :config
    (setq company-box-backends-colors nil)
    (setq company-box-show-single-candidate t)
    (setq company-box-max-candidates 50)

    (defun company-box-icons--elisp (candidate)
      (when (derived-mode-p 'emacs-lisp-mode)
        (let ((sym (intern candidate)))
          (cond ((fboundp sym) 'Function)
                ((featurep sym) 'Module)
                ((facep sym) 'Color)
                ((boundp sym) 'Variable)
                ((symbolp sym) 'Text)
                (t . nil)))))
    (with-eval-after-load 'all-the-icons
      (declare-function all-the-icons-faicon "all-the-icons" ())
      (declare-function all-the-icons-fileicon "all-the-icons" ())
      (declare-function all-the-icons-material "all-the-icons" ())
      (declare-function all-the-icons-octicon "all-the-icons" ())
      (setq company-box-icons-all-the-icons
            `((Unknown . ,(all-the-icons-material "find_in_page" :height 0.7 :v-adjust -0.15))
              (Text . ,(all-the-icons-faicon "book" :height 0.68 :v-adjust -0.15))
              (Method . ,(all-the-icons-faicon "cube" :height 0.7 :v-adjust -0.05 :face 'font-lock-constant-face))
              (Function . ,(all-the-icons-faicon "cube" :height 0.7 :v-adjust -0.05 :face 'font-lock-constant-face))
              (Constructor . ,(all-the-icons-faicon "cube" :height 0.7 :v-adjust -0.05 :face 'font-lock-constant-face))
              (Field . ,(all-the-icons-faicon "tags" :height 0.65 :v-adjust -0.15 :face 'font-lock-warning-face))
              (Variable . ,(all-the-icons-faicon "tag" :height 0.7 :v-adjust -0.05 :face 'font-lock-warning-face))
              (Class . ,(all-the-icons-faicon "clone" :height 0.65 :v-adjust 0.01 :face 'font-lock-constant-face))
              (Interface . ,(all-the-icons-faicon "clone" :height 0.65 :v-adjust 0.01))
              (Module . ,(all-the-icons-octicon "package" :height 0.7 :v-adjust -0.15))
              (Property . ,(all-the-icons-octicon "package" :height 0.7 :v-adjust -0.05 :face 'font-lock-warning-face)) ;; Golang module
              (Unit . ,(all-the-icons-material "settings_system_daydream" :height 0.7 :v-adjust -0.15))
              (Value . ,(all-the-icons-material "format_align_right" :height 0.7 :v-adjust -0.15 :face 'font-lock-constant-face))
              (Enum . ,(all-the-icons-material "storage" :height 0.7 :v-adjust -0.15 :face 'all-the-icons-orange))
              (Keyword . ,(all-the-icons-material "filter_center_focus" :height 0.7 :v-adjust -0.15))
              (Snippet . ,(all-the-icons-faicon "code" :height 0.7 :v-adjust 0.02 :face 'font-lock-variable-name-face))
              (Color . ,(all-the-icons-material "palette" :height 0.7 :v-adjust -0.15))
              (File . ,(all-the-icons-faicon "file-o" :height 0.7 :v-adjust -0.05))
              (Reference . ,(all-the-icons-material "collections_bookmark" :height 0.7 :v-adjust -0.15))
              (Folder . ,(all-the-icons-octicon "file-directory" :height 0.7 :v-adjust -0.05))
              (EnumMember . ,(all-the-icons-material "format_align_right" :height 0.7 :v-adjust -0.15 :face 'all-the-icons-blueb))
              (Constant . ,(all-the-icons-faicon "tag" :height 0.7 :v-adjust -0.05))
              (Struct . ,(all-the-icons-faicon "clone" :height 0.65 :v-adjust 0.01 :face 'font-lock-constant-face))
              (Event . ,(all-the-icons-faicon "bolt" :height 0.7 :v-adjust -0.05 :face 'all-the-icons-orange))
              (Operator . ,(all-the-icons-fileicon "typedoc" :height 0.65 :v-adjust 0.05))
              (TypeParameter . ,(all-the-icons-faicon "hashtag" :height 0.65 :v-adjust 0.07 :face 'font-lock-const-face))
              (Template . ,(all-the-icons-faicon "code" :height 0.7 :v-adjust 0.02 :face 'font-lock-variable-name-face)))))
  )
  ;; Show quick tooltip
  (use-package company-quickhelp
    :defines company-quickhelp-delay
    :bind (:map company-active-map
	("M-h" . company-quickhelp-manual-begin))
    :hook (global-company-mode . company-quickhelp-mode)
    :custom (company-quickhelp-delay 0.8))
)

;; git
;; Timemachine
(use-package git-timemachine
  :bind("M-g t" . git-timemachine-toggle))

;; Diffview
(use-package diffview
  :commands (diffview-region diffview-current)
  :preface
  (defun espinosa/diffview-dwim()
    (interactive)
    (if (region-active-p)
        (diffview-region)
      (diffview-current))
    )
  :bind ("M-g v" . espinosa/diffview-dwim)
  )

;; Magit
(use-package magit
  :custom
  (magit-auto-revert-mode nil)
  :bind
  ("M-g s" . magit-status)
  )

;; GitModes
(use-package gitattributes-mode :defer t)
(use-package gitconfig-mode :defer t)
(use-package gitignore-mode :defer t)

;; Git Gutter
(use-package git-gutter
    :custom
    (git-gutter:modified-sign "~")		; 
    (git-gutter:added-sign    "+")		; 
    (git-gutter:deleted-sign  "-")		; 
    :custom-face
    (git-gutter:modified ((t (:foreground "#f1fa8c" :background "#f1fa8c"))))
    (git-gutter:added    ((t (:foreground "#50fa7b" :background "#50fa7b"))))
    (git-gutter:deleted  ((t (:foreground "#ff79c6" :background "#ff79c6"))))
    :config
    (global-git-gutter-mode +1))

;; Git Remote
(use-package browse-at-remote
  :bind ("M-g r" . browse-at-remote))

;; GitHub
(use-package github-pullrequest)

;; smerge
(use-package smerge-mode
  :diminish
  :preface
  (with-eval-after-load 'hydra
    (defhydra smerge-hydra
      (:color pink :hint nil :post (smerge-auto-leave))
      "
^Move^       ^Keep^               ^Diff^                 ^Other^
^^-----------^^-------------------^^---------------------^^-------
_n_ext       _b_ase               _<_: upper/base        _C_ombine
_p_rev       _u_pper              _=_: upper/lower       _r_esolve
^^           _l_ower              _>_: base/lower        _k_ill current
^^           _a_ll                _R_efine
^^           _RET_: current       _E_diff
"
      ("n" smerge-next)
      ("p" smerge-prev)
      ("b" smerge-keep-base)
      ("u" smerge-keep-upper)
      ("l" smerge-keep-lower)
      ("a" smerge-keep-all)
      ("RET" smerge-keep-current)
      ("\C-m" smerge-keep-current)
      ("<" smerge-diff-base-upper)
      ("=" smerge-diff-upper-lower)
      (">" smerge-diff-base-lower)
      ("R" smerge-refine)
      ("E" smerge-ediff)
      ("C" smerge-combine-with-next)
      ("r" smerge-resolve)
      ("k" smerge-kill-current)
      ("ZZ" (lambda ()
              (interactive)
              (save-buffer)
              (bury-buffer))
       "Save and bury buffer" :color blue)
      ("q" nil "cancel" :color blue)))
  :hook ((find-file . (lambda ()
                        (save-excursion
                          (goto-char (point-min))
                          (when (re-search-forward "^<<<<<<< " nil t)
                            (smerge-mode 1)))))
         (magit-diff-visit-file . (lambda ()
                                    (when smerge-mode
                                      (smerge-hydra/body)))))

)

(use-package plantuml-mode
  :custom
  (plantuml-jar-path "~/.emacs.d/plantuml.jar")
  :mode "\\.uml\\'")

;; ob-async
(use-package async)
(use-package ob-async
  :load-path "/home/espinosa/Google_Drive/fractaliusfciencias/Emacs/src/github.com/astahlam/ob-async"
  :config
  (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)
  (add-hook 'ob-async-pre-execute-src-block-hook
      '(lambda ()                       
         (setq org-plantuml-jar-path "~/.emacs.d/plantuml.jar"))))

;; Screenshot
(defun my-org-screenshot ()
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

;; Evil Mode
(use-package evil
  :ensure t
  :config
  (evil-mode 1)
  (use-package evil-org
    :ensure t
    :config
    (evil-org-set-key-theme
     '(textobjects insert navigation additional shift todo heading))
    (add-hook 'org-mode-hook (lambda () (evil-org-mode)))
    )

)

;; LaTeX
(defvar my/bib-file-location "~/Google_Drive/fractaliusfciencias/Bib/library.bib"
  "Where I keep my bib file")

(use-package tex-site
  :ensure auctex
  :mode ("\\.tex\\'" . latex-mode)
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
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
  (setq bibtex-completion-notes-path "~/Google_Drive/fractaliusfciencias/Bib/ref.org")
  (setq bibtex-completion-pdf-field "File")
  (setq bibtex-completion-library-path '("~/Google_Drive/fractaliusfciencias/Bib/Papers" "~/Google_Drive/fractaliusfciencias/Bib/Libros"))
  (setq ivy-bibtex-default-action #'ivy-bibtex-insert-citation)
  (ivy-set-actions
   'ivy-bibtex
   '(("p" ivy-bibtex-open-any "Open PDF, URL, or DOI")
     ("e" ivy-bibtex-edit-notes "Edit notes")))
  (defun bibtex-completion-open-pdf-external (keys &optional fallback-action)
    (let ((bibtex-completion-pdf-open-function
           (lambda (fpath) (start-process "evince" "*helm-bibtex-evince*" "/usr/bin/evince" fpath))))
      (bibtex-completion-open-pdf fallback-action)))
  (ivy-bibtex-ivify-action bibtex-completion-open-pdf-external ivy-bibtex-open-pdf-external)
  (ivy-add-actions
   'ivy-bibtex
   '(("P" ivy-bibtex-open-pdf-external "Open PDF file in external viewer (if present)")))
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

;; Org
  (use-package org
    :init
    (setq private-directory "/home/espinosa/Google_Drive/fractaliusfciencias/Org/")
    (setq task-file (concat private-directory "task.org"))
    (setq schedule-file (concat private-directory "schedule.org")) ;
    (setq org-agenda-start-day "-3d")
    :custom
    (org-src-fontify-natively t)
    (org-directory "/home/espinosa/Google_Drive/fractaliusfciencias/Org/")
    (org-ditaa-jar-path "/usr/bin/ditaa")
    (org-plantuml-jar-path "~/.emacs.d/plantuml.jar")
    (org-confirm-babel-evaluate nil)
    (org-clock-out-remove-zero-time-clocks t)
    (org-startup-folded 'content)
    (org-columns-default-format "%50ITEM(Task) %5TODO(Todo) %10Effort(Effort){:} %10CLOCKSUM(Clock) %2PRIORITY %TAGS")
    (org-agenda-columns-add-appointments-to-effort-sum t)
    (org-agenda-span '10)
    (org-agenda-log-mode-items (quote (closed clock)))
    (org-agenda-clockreport-parameter-plist
      '(:maxlevel 5 :block t :tstart t :tend t :emphasize t :link t :narrow 80 :indent t :formula nil :timestamp t :level 5 :tcolumns nil :formatter nil))
    (org-global-properties (quote ((
      "Effort_ALL" . "00:05 00:10 00:15 00:30 01:00 01:30 02:00 02:30 03:00"))))
    (org-agenda-files (quote (
       "/home/espinosa/Google_Drive/fractaliusfciencias/Org/task.org"
       "/home/espinosa/Google_Drive/fractaliusfciencias/Org/routine.org"
       "/home/espinosa/Google_Drive/fractaliusfciencias/Org/schedule.org")))
    :custom-face
    (org-link ((t (:foreground "#ebe087" :underline t))))
    (org-list-dt ((t (:foreground "#bd93f9"))))
    (org-special-keyword ((t (:foreground "#6272a4"))))
    (org-todo ((t (:background "#272934" :foreground "#51fa7b" :weight bold))))
    (org-document-title ((t (:foreground "#f1fa8c" :weight bold))))
    (org-done ((t (:background "#373844" :foreground "#216933" :strike-through nil :weight bold))))
    (org-footnote ((t (:foreground "#76e0f3"))))
    ;; do not scale outline header
    ;; (org-level-1 ((t (:inherit outline-1 :height 1.0))))
    ;; (org-level-2 ((t (:inherit outline-2 :height 1.0))))
    ;; (org-level-3 ((t (:inherit outline-3 :height 1.0))))
    ;; (org-level-4 ((t (:inherit outline-4 :height 1.0))))
    ;; (org-level-5 ((t (:inherit outline-5 :height 1.0))))
    :bind (("M-o c" . counsel-org-capture)
           ("M-o a" . org-agenda)
           ("C-x C-l" . org-store-link)
           :map org-mode-map
           ("C-c i" . org-clock-in)
           ("C-c o" . org-clock-out)
           ("C-c n" . org-narrow-to-subtree)
           ("C-c b" . org-narrow-to-block)
           ("C-c w" . widen)
           ("C-c e" . org-set-effort)
           ;; custom functions
           ("C-c 1" . (lambda () (interactive) (org-cycle-list-bullet 2)))
           ("M-o l i" . (lambda () (interactive) (espinosa/open-org-file task-file)))
           ("M-o l s" . (lambda () (interactive) (espinosa/open-org-file schedule-file)))
           ("M-o l y" . (lambda () (interactive) (espinosa/open-org-file (espinosa/get-yesterday-diary))))
           ("M-o l p" . (lambda () (interactive) (espinosa/open-org-file (espinosa/get-diary-from-cal))))
           ("M-o l t" . (lambda () (interactive) (espinosa/open-org-file (espinosa/get-today-diary)))))
    :hook
    (kill-emacs . espinosa/org-clock-out-and-save-when-exit)
    (org-clock-in .
                (lambda ()
                  (setq org-mode-line-string (espinosa/task-clocked-time))
                  (run-at-time 0 60 '(lambda ()
                                       (setq org-mode-line-string (espinosa/task-clocked-time))
                                       (force-mode-line-update)))
                  (force-mode-line-update)))
    (org-mode . (lambda ()
                       (dolist (key '("C-'" "C-," "C-."))
                         (unbind-key key org-mode-map))))
    :preface
    (defun espinosa/get-today-diary ()
      (concat private-directory
        (format-time-string "diary/%Y/%m/%Y-%m-%d.org" (current-time))))
    (defun espinosa/get-yesterday-diary ()
      (concat private-directory
        (format-time-string "diary/%Y/%m/%Y-%m-%d.org" (time-add (current-time) (* -24 3600)))))
    (defun espinosa/get-diary-from-cal ()
      (concat private-directory
        (format-time-string "diary/%Y/%m/%Y-%m-%d.org"
          (apply 'encode-time (parse-time-string (concat (org-read-date) " 00:00"))))))
    (defun espinosa/open-org-file (fname)
      (switch-to-buffer (find-file-noselect fname)))
    (defun espinosa/org-get-time ()
      (format-time-string "<%H:%M>" (current-time)))
    (defun espinosa/code-metadata ()
      (concat ":" (projectile-project-name) ":"))
    (defun espinosa/org-clock-out-and-save-when-exit ()
        "Save buffers and stop clocking when kill emacs."
          (ignore-errors (org-clock-out) t)
          (save-some-buffers t))
    (defun espinosa/task-clocked-time ()
        "Return a string with the clocked time and effort, if any"
        (interactive)
        (let* ((clocked-time (org-clock-get-clocked-time))
               (h (truncate clocked-time 60))
               (m (mod clocked-time 60))
               (work-done-str (format "%d:%02d" h m)))
          (if org-clock-effort
              (let* ((effort-in-minutes
                      (org-duration-to-minutes org-clock-effort))
                     (effort-h (truncate effort-in-minutes 60))
                     (effort-m (truncate (mod effort-in-minutes 60)))
                     (effort-str (format "%d:%02d" effort-h effort-m)))
                (format "%s/%s" work-done-str effort-str))
            (format "%s" work-done-str))))
    :config
    ;; Pomodoro
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
      :hook
      (org-pomodoro-started . (lambda () (notifications-notify
                                          :title "org-pomodoro"
                                          :body "Let's focus for 25 minutes!"
                                          :app-icon "/home/espinosa/.config/emacs/img/001-food-and-restaurant.png")))
      (org-pomodoro-finished . (lambda () (notifications-notify
                                           :title "org-pomodoro"
                                           :body "Well done! Take a break."
                                           :app-icon "/home/espinosa/.config/emacs/img/004-beer.png")))
      :config
      (when (eq system-type 'darwin)
      (setq alert-default-style 'osx-notifier))
      (require 'alert)
      :bind (:map org-agenda-mode-map
                  ("p" . org-pomodoro)))
    (setq org-agenda-current-time-string "← now")
    (setq org-agenda-time-grid ;; Format is changed from 9.1
          '((daily today require-timed)
            (0900 01000 1100 1200 1300 1400 1500 1600 1700 1800 1900 2000 2100 2200 2300 2400)
            "-"
            "────────────────"))
    (setq org-todo-keyword-faces
          '(("WAIT" . (:foreground "#6272a4":weight bold))
            ("NEXT"   . (:foreground "#f1fa8c" :weight bold))
            ("CARRY/O" . (:foreground "#6272a4" :background "#373844" :weight bold))))
    (setq org-capture-templates
          '(("tweet" "Write down the thoughts of this moment with a timestamp." item
             (file+headline espinosa/get-today-diary "Log")
             "%(espinosa/org-get-time) %? \n")
            ;; memo
            ("memo" "Memorize something in the memo section of today's diary." entry
             (file+headline espinosa/get-today-diary "Memo")
             "** %? \n"
             :unnarrowed 1)
            ;; tasks
            ("inbox" "Create a general task to the inbox and jump to the task file." entry
             (file+headline "/home/espinosa/Google_Drive/fractaliusfciencias/Org/task.org" "Inbox")
             "** TODO %?"
             :jump-to-captured 1)
            ("interrupt-task" "Create an interrupt task to the inbox and start clocking." entry
             (file+headline "/home/espinosa/Google_Drive/fractaliusfciencias/Org/task.org" "Inbox")
             "** TODO %?"
             :jump-to-captured 1 :clock-in 1 :clock-resume 1)
            ("hack-emacs" "Collect hacking Emacs ideas!" item
             (file+headline "/home/espinosa/Google_Drive/fractaliusfciencias/Org/task.org" "Hacking Emacs")
             "- [ ] %?"
             :prepend t)
            ("private-schedule" "Add an event to the private calendar." entry
             (file+olp schedule-file "Calendar" "2021" "Private")
             "** %?\n   SCHEDULED: <%(org-read-date)>\n"
             :prepend t)
            ("work-schedule" "Add an event to the work calendar." entry
             (file+olp schedule-file "Calendar" "2021" "Work")
             "** %?\n   SCHEDULED: <%(org-read-date)>\n")
            ("store-link" "Store the link of the current position in the clocking task." item
             (clock)
             "- %A\n"
             :immediate t :prepend t)
            ;; code-reading
            ("code-link" "Store the code reading memo to today's diary with metadata." entry
             (file+headline espinosa/get-today-diary "Code")
             ;;(file+headline ladicle/get-today-diary "Code")
             "** %? %(espinosa/code-metadata)\n%A\n")))
    (setq org-modules (delete '(org-gnus org-w3m org-bbdb org-bibtex org-docview org-info org-irc org-mhe org-rmail org-eww) org-modules))
    ;; load org-protocol for capturing websites
    (use-package org-protocol
      :ensure nil)
    ;; load babel languages
    (org-babel-do-load-languages
         'org-babel-load-languages
         '((dot . t)
           (plantuml . t)
           (latex . t)
           (R . t)))
    ;; Pretty bullets
    (use-package org-bullets
      :custom (org-bullets-bullet-list '("" "" "" "" "" "" "" "" "" ""))
      :hook (org-mode . org-bullets-mode))
    (use-package ox-hugo
      :after ox
      :custom
      (org-blackfriday--org-element-string '((src-block . "Code")
                                             (table . "Table")
                                             (figure . "Figure"))))
    ;; Download Drag&Drop images
    (use-package org-download)

;;     (with-eval-after-load 'hydra
;;       (eval-and-compile
;;         (defun hot-expand (str &optional mod)
;;           "Expand org template."
;;           (let (text)
;;             (when (region-active-p)
;; 	            (setq text (buffer-substring (region-beginning) (region-end)))
;; 	             (delete-region (region-beginning) (region-end)))
;; 	    (insert str)
;; 	    (org-try-structure-completion)
;; 	    (when mod (insert mod) (forward-line))
;; 	    (when text (insert text)))))
;;       (defhydra hydra-org-template (:color blue :hint nil)
;;       (format "   %s^^     %s^^^^       %s^^^^^        %s
;; %s
;;   %s _e_lisp      %s _h_ugo        %s plant_u_ml      %s _s_ource
;;   %s _r_uby       %s _c_aption     %s La_t_ex         %s _n_ote
;;   %s _f_ish       %s _l_ink        %s i_P_ython       %s _i_nfo
;;   %s _b_ash       %s %s^^^^^       %s %s^^^^          %s qu_o_te
;;   %s _g_o
;;   %s _y_aml
;; %s
;;                      %s quit%s insert
;; "
;; 	(concat (propertize "((" 'face `(:foreground "#6272a4"))
;; 		(propertize "CODE" 'face `(:foreground "#ff79c6" :weight bold))
;;                 (propertize "))" 'face `(:foreground "#6272a4")))
;; 	(concat (propertize "((" 'face `(:foreground "#6272a4"))
;; 	        (propertize "META" 'face `(:foreground "#ff79c6" :weight bold))
;;                 (propertize "))" 'face `(:foreground "#6272a4")))
;; 	(concat (propertize "((" 'face `(:foreground "#6272a4"))
;; 	        (propertize "DRAW" 'face `(:foreground "#ff79c6" :weight bold))
;; 		(propertize "))" 'face `(:foreground "#6272a4")))
;; 	(concat (propertize "((" 'face `(:foreground "#6272a4"))
;; 	        (propertize "BLOCK" 'face `(:foreground "#ff79c6" :weight bold))
;; 	        (propertize "))" 'face `(:foreground "#6272a4")))
;; 	(propertize " ──────────────────────────────────────────────────────────── " 'face `(:foreground "#6272a4"))
;; 	;; L1
;; 	(all-the-icons-fileicon "emacs" :v-adjust .00001 :height .68 :face '(:foreground "#6272a4"))
;; 	(all-the-icons-material "web" :v-adjust -.1 :height .7 :face '(:foreground "#6272a4"))
;; 	(all-the-icons-material "format_shapes" :v-adjust -.15 :height .7 :face '(:foreground "#6272a4"))
;; 	(all-the-icons-octicon "code" :v-adjust -.05 :height .75  :face '(:foreground "#6272a4"))
;; 	;; L2
;; 	(all-the-icons-alltheicon "ruby-alt" :v-adjust .0505 :height .7 :face '(:foreground "#6272a4"))
;; 	(all-the-icons-faicon "flag" :v-adjust -.05 :height .69  :face '(:foreground "#6272a4"))
;; 	(all-the-icons-faicon "text-height" :v-adjust -.05 :height .69 :face '(:foreground "#6272a4"))
;; 	(all-the-icons-octicon "light-bulb" :v-adjust -.1 :height .78 :face '(:foreground "#6272a4"))
;; 	;; L3
;; 	(all-the-icons-alltheicon "script" :v-adjust .05 :height .7 :face '(:foreground "#6272a4"))
;; 	(all-the-icons-faicon "link" :v-adjust -.05 :height .69 :face '(:foreground "#6272a4"))
;; 	(all-the-icons-fileicon "test-python" :v-adjust -.1 :height .75 :face '(:foreground "#6272a4"))
;; 	(all-the-icons-faicon "info-circle" :v-adjust -.1 :height .72 :face '(:foreground "#6272a4"))
;; 	;; L4
;; 	(all-the-icons-alltheicon "script" :v-adjust .05 :height .7 :face '(:foreground "#6272a4"))
;; 	(all-the-icons-fileicon "test-python" :v-adjust -.1 :height .7 :face '(:foreground "#282a36")) ;; dummy
;; 	(propertize "link" 'face `(:foreground "#282a36"))
;; 	(all-the-icons-fileicon "test-python" :v-adjust -.1 :height .7 :face '(:foreground "#282a36")) ;; dummy
;; 	(propertize "latex" 'face `(:foreground "#282a36"))
;; 	(all-the-icons-faicon "quote-right" :v-adjust -.05 :height .65 :face '(:foreground "#6272a4"))
;; 	;; L5
;; 	(all-the-icons-fileicon "go" :v-adjust -.1 :height .75 :face '(:foreground "#6272a4"))
;; 	;; L6
;; 	(all-the-icons-octicon "settings" :v-adjust -.1 :height .75 :face '(:foreground "#6272a4"))
;; 	;; Draw
;; 	(propertize " ┌──────────────────────────────────────────────────────────┘ " 'face `(:foreground "#6272a4"))
;; 	(propertize "[_q_]:" 'face `(:foreground "#6272a4"))
;; 	(propertize ", [_<_]:" 'face `(:foreground "#6272a4"))
;;     )
;;     ("s" (hot-expand "<s"))
;;     ("o" (hot-expand "<q"))
;;     ("c" (hot-expand "<c"))
;;     ("t" (hot-expand "<L"))
;;     ("c" (insert "#+CAPTION: "))
;;     ("l" (insert "#+NAME: "))
;;     ("n" (insert "#+BEGIN_NOTE\n\n#+END_NOTE"))
;;     ("i" (insert "#+BEGIN_INFO\n\n#+END_INFO"))
;;     ("h" (insert ":PROPERTIES:\n:EXPORT_FILE_NAME:\n:EXPORT_HUGO_SECTION: pages\n:EXPORT_HUGO_TAGS:\n:EXPORT_HUGO_CATEGORIES:\n:END:"))
;;     ("e" (hot-expand "<s" "emacs-lisp"))
;;     ("f" (hot-expand "<s" "fish"))
;;     ("b" (hot-expand "<s" "bash"))
;;     ("y" (hot-expand "<s" "yaml"))
;;     ("P" (hot-expand "<s" "ipython :session :exports both :async :cache yes :results raw drawer\n$0"))
;;     ("g" (hot-expand "<s" "go"))
;;     ("r" (hot-expand "<s" "ruby"))
;;     ("S" (hot-expand "<s" "sh"))
;;     ("u" (hot-expand "<s" "plantuml :file overview.svg :cache yes :cmdline -config \"$HOME/Private/style.uml\" :async"))
;;     ("<" self-insert-command)
;;     ("q" nil)
;;   )
;;   (bind-key "<"
;; 	(lambda () (interactive)
;;         	(if (or (region-active-p) (looking-back "^\s*" 1))
;;                 	(hydra-org-template/body)
;;                 	(self-insert-command 1)
;;                 )
;;         )
;;   org-mode-map)
;;   )
)                                       

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)))

(use-package dashboard
  :diminish
  (dashboard-mode page-break-lines-mode)
  :custom
  (dashboard-center-content t)
  (dashboard-startup-banner 'logo)
  (dashboard-items '((recents . 15)
                     (projects . 5)
                     (bookmarks . 5)))
  :custom-face
  (dashboard-heading ((t (:foreground "#f1fa8c" :weight bold))))
  :hook
  (after-init . dashboard-setup-startup-hook))

;; Change cursor style
(add-to-list 'default-frame-alist '(cursor-type . bar))
;; vertical border
(let ((display-table (or standard-display-table (make-display-table))))
  (set-display-table-slot display-table 'vertical-border (make-glyph-code ?│)) ; or ┃ │
  (setq standard-display-table display-table))
(set-face-background 'vertical-border "#0e0f1b")
(set-face-foreground 'vertical-border (face-background 'vertical-border))

(use-package persp-mode
  :disabled
  :diminish
  :commands (get-current-persp persp-contain-buffer-p)
  :hook (after-init . persp-mode))

(use-package imenu-list
  :load-path "/home/espinosa/Google_Drive/fractaliusfciencias/Emacs/src/github.com/Ladicle/imenu-list"
  :bind
  ("<f10>" . imenu-list-smart-toggle)
  :custom-face
  (imenu-list-entry-face-1 ((t (:foreground "white"))))
  :custom
  (imenu-list-focus-after-activation t)
  (imenu-list-auto-resize t))

(defun espinosa/toggle-window-transparency ()
  "Cycle the frame transparency from default to transparent."
  (interactive)
  (let ((transparency 85)
        (opacity 100))
    (if (and (not (eq (frame-parameter nil 'alpha) nil))
             (< (frame-parameter nil 'alpha) opacity))
        (set-frame-parameter nil 'alpha opacity)
      (set-frame-parameter nil 'alpha transparency))))

(global-set-key (kbd "M-t t") 'espinosa/toggle-window-transparency)

(use-package minimap
  :commands
  (minimap-bufname minimap-create minimap-kill)
  :custom
  (minimap-major-modes '(prog-mode))
  (minimap-window-location 'right)
  (minimap-update-delay 0.2)
  (minimap-minimum-width 20)
  :bind
  ("M-t p" . espinosa/toggle-minimap)
  :preface
  (defun espinosa/toggle-minimap ()
    "Toggle minimap for current buffer."
    (interactive)
    (if (null minimap-bufname)
        (minimap-create)
      (minimap-kill)))
  :config
  (custom-set-faces
   '(minimap-active-region-background
    ((((background dark)) (:background "#555555555555"))
      (t (:background "#C847D8FEFFFF"))) :group 'minimap)))

(use-package neotree
  :after
  projectile
  :commands
  (neotree-show neotree-hide neotree-dir neotree-find)
  :custom
  (neo-theme 'nerd2)
  :bind
  ("<f8>" . neotree-current-dir-toggle)
  ("<f9>" . neotree-projectile-toggle)
  :preface
  (defun neotree-projectile-toggle ()
    (interactive)
    (let ((project-dir
           (ignore-errors
           ;;; Pick one: projectile or find-file-in-project
             (projectile-project-root)
             ))
          (file-name (buffer-file-name))
          (neo-smart-open t))
      (if (and (fboundp 'neo-global--window-exists-p)
               (neo-global--window-exists-p))
          (neotree-hide)
        (progn
          (neotree-show)
          (if project-dir
              (neotree-dir project-dir))
          (if file-name
              (neotree-find file-name))))))
  (defun neotree-current-dir-toggle ()
    (interactive)
    (let ((project-dir
           (ignore-errors
             (ffip-project-root)
             ))
          (file-name (buffer-file-name))
          (neo-smart-open t))
      (if (and (fboundp 'neo-global--window-exists-p)
               (neo-global--window-exists-p))
          (neotree-hide)
        (progn
          (neotree-show)
          (if project-dir
              (neotree-dir project-dir))
          (if file-name
              (neotree-find file-name)))))))

;; Vterm
(use-package vterm
    :ensure t)

(use-package doom-themes
  :custom
  (doom-themes-enable-italic t)
  (doom-themes-enable-bold t)
  :custom-face
  ;; (vertical-bar   (doom-darken base5 0.4))
  ;; (doom-darken bg 0.4)
  :config
  (load-theme 'doom-dracula t)
  (doom-themes-neotree-config)
  (doom-themes-org-config)
  ;; Modeline
  (use-package doom-modeline
    :custom
    (doom-modeline-buffer-file-name-style 'truncate-with-project)
    (doom-modeline-icon t)
    (doom-modeline-major-mode-icon nil)
    (doom-modeline-minor-modes nil)
    :hook
    (after-init . doom-modeline-mode)
    :config
    (with-eval-after-load 'evil
      (doom-modeline-def-segment evil-state
        "The current evil state.  Requires `evil-mode' to be enabled."
        (when (bound-and-true-p evil-local-mode)
          (s-trim-right (evil-state-property evil-state :tag t)))
       )
     )
    (setq evil-normal-state-tag   (propertize "[Normal]" 'face '((:background "green" :foreground "black")))
          evil-emacs-state-tag    (propertize "[Emacs]" 'face '((:background "orange" :foreground "black")))
          evil-insert-state-tag   (propertize "[Insert]" 'face '((:background "red") :foreground "white"))
          evil-motion-state-tag   (propertize "[Motion]" 'face '((:background "blue") :foreground "white"))
          evil-visual-state-tag   (propertize "[Visual]" 'face '((:background "grey80" :foreground "black")))
          evil-operator-state-tag (propertize "[Operator]" 'face '((:background "purple")))
          )
    (set-cursor-color "cyan")
    (line-number-mode 0)
    (column-number-mode 0)
    (doom-modeline-def-modeline 'main 
	'(bar workspace-name window-number evil-state buffer-info buffer-position selection-info)
	'(misc-info persp-name debug minor-modes input-method major-mode))
    ;;(doom-modeline-def-modeline 'main
    ;;  '(bar workspace-number window-number evil-state god-state ryo-modal xah-fly-keys matches buffer-info remote-host buffer-position parrot selection-info)
    ;;  '(misc-info persp-name lsp github debug minor-modes input-method major-mode process vcs checker))
    ))

(use-package nyan-mode
   :custom
   (nyan-cat-face-number 4)
   (nyan-animate-nyancat t)
   :hook
   (doom-modeline-mode . nyan-mode))

(use-package hide-mode-line
  :hook
  ((neotree-mode imenu-list-minor-mode minimap-mode) . hide-mode-line-mode))

(use-package display-line-numbers
  :ensure nil
  :hook
  ((prog-mode yaml-mode systemd-mode) . display-line-numbers-mode))

(use-package dimmer
  :disabled
  :custom
  (dimmer-fraction 0.5)
  (dimmer-exclusion-regexp-list
       '(".*Minibuf.*"
         ".*which-key.*"
         ".*NeoTree.*"
         ".*Messages.*"
         ".*Async.*"
         ".*Warnings.*"
         ".*LV.*"
         ".*Ilist.*"))
  :config
  (dimmer-mode t))

(use-package fill-column-indicator
  :hook
  ((markdown-mode
    git-commit-mode) . fci-mode))

(use-package presentation)

(setq eshell-prompt-function
      (lambda ()
        (format "%s %s\n%s%s%s "
                (all-the-icons-octicon "repo")
                (propertize (cdr (shrink-path-prompt default-directory)) 'face `(:foreground "white"))
                (propertize "❯" 'face `(:foreground "#ff79c6"))
                (propertize "❯" 'face `(:foreground "#f1fa8c"))
                (propertize "❯" 'face `(:foreground "#50fa7b")))))

(setq eshell-hist-ignoredups t)
(setq eshell-cmpl-cycle-completions nil)
(setq eshell-cmpl-ignore-case t)
(setq eshell-ask-to-save-history (quote always))
(setq eshell-prompt-regexp "❯❯❯ ")
(add-hook 'eshell-mode-hook
          '(lambda ()
             (progn
               (define-key eshell-mode-map "\C-a" 'eshell-bol)
               (define-key eshell-mode-map "\C-r" 'counsel-esh-history)
               (define-key eshell-mode-map [up] 'previous-line)
               (define-key eshell-mode-map [down] 'next-line)
               )))

(use-package hl-line
  :ensure nil
  :hook
  (after-init . global-hl-line-mode))

(use-package paren
  :ensure nil
  :hook
  (after-init . show-paren-mode)
  :custom-face
  (show-paren-match ((nil (:background "#44475a" :foreground "#f1fa8c")))) ;; :box t
  :custom
  (show-paren-style 'mixed)
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t))

(use-package highlight-symbol
  :bind
  (:map prog-mode-map
  ("M-o h" . highlight-symbol)
  ("M-p" . highlight-symbol-prev)
  ("M-n" . highlight-symbol-next)))

(use-package beacon
  :custom
  (beacon-color "#f1fa8c")
  :hook (after-init . beacon-mode))

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  :diminish
  :hook (emacs-lisp-mode . rainbow-mode))

(use-package volatile-highlights
  :diminish
  :hook
  (after-init . volatile-highlights-mode)
  :custom-face
  (vhl/default-face ((nil (:foreground "#FF3333" :background "#FFCDCD")))))

(use-package highlight-indent-guides
  :diminish
  :hook
  ((prog-mode yaml-mode) . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-auto-enabled t)
  (highlight-indent-guides-responsive t)
  (highlight-indent-guides-method 'character)) ; column

(setq custom-file "~/.config/emacs/custom.el")
(load custom-file 'noerror)

