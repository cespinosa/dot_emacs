;; Load directory for configuration files for emacs
(add-to-list 'load-path (expand-file-name "ConfigFiles" user-emacs-directory))

;; Set home and emacs dirs
(defvar user-home-directory (concat (getenv "HOME") "/"))
(setq user-emacs-directory (concat user-home-directory ".emacs.d/"))

;; load general configuration
(load (locate-user-emacs-file "general.el") nil :nomessage)

(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)
(setq initial-scratch-message nil)

;; Load all use-package related configuration
(load (locate-user-emacs-file "SetupPackage.el") nil :nomessage)

(require 'SetupOptimizations)
(require 'SetupTheme)
(require 'SetupEditor)
(require 'SetupEvil)
(require 'SetupFlycheck)
(require 'SetupGit)
(require 'SetupHighlight)
(require 'SetupIvy)
(require 'SetupModeline)
(require 'SetupPython)
(require 'SetupFont)
(require 'SetupSmartparents)
(require 'SetupVisual)
(require 'SetupWhichkey)
(require 'SetupWindows)
