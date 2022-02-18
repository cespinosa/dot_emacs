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
(require 'SetupNoLittering)
(require 'SetupOrg)
(require 'SetupTheme)
(require 'SetupDashboard)
(require 'SetupEditor)
(require 'SetupEvil)
(require 'SetupFlycheck)
(require 'SetupFont)
(require 'SetupGit)
(require 'SetupHighlight)
(require 'SetupIBuffer)
(require 'SetupIvy)
(require 'SetupCounsel)
(require 'SetupLatex)
(require 'SetupMarkdown)
(require 'SetupModeline)
(require 'SetupPlantuml)
(require 'SetupProject)
(require 'SetupPython)
(require 'SetupSmartparents)
(require 'SetupVisual)
(require 'SetupVterm)
(require 'SetupWhichkey)
(require 'SetupWindows)

;; start emacs server only it has not already been started
(require 'server)
(unless (server-running-p) (server-start))
