;; Emacs settings
;; Author: 	Lei Liu <liunx163@163.com>
;; License: 	GPL

;; font settings
(set-face-attribute 'default nil :font "Monospace 12")
;; don't display tool bar
(tool-bar-mode -1)
;; don't display menu bar
(menu-bar-mode -1)
;; don't display scroll bar
(scroll-bar-mode -1)

;; turn on ido mode easy for file dir search
(require 'ido)
(ido-mode t)

(add-to-list 'load-path "~/.emacs.d/")
;; ==========================================================================
;; color-theme select settings
;; ==========================================================================
(add-to-list 'load-path "~/.emacs.d/color_themes/")
(require 'color-theme)
(setq color-theme-is-global t)
(color-theme-initialize)

(load "color-theme-colorful-obsolescence")
(load "color-theme-zenburn")
(load "color-theme-tangotango")
(load "color-theme-wombat")
(load "color-theme-leuven")
(load "color-theme-folio")
(load "color-theme-zenash")
(load "color-theme-manoj")

(setq my-color-themes (list
  'color-theme-tangotango
  'color-theme-colorful-obsolescence 
  'color-theme-zenburn
  'color-theme-leuven 
  'color-theme-folio 
  'color-theme-manoj 
  'color-theme-zenash
  'color-theme-wombat
))

(defun my-theme-set-default () ; Set the first row
      (interactive)
      (setq theme-current my-color-themes)
      (funcall (car theme-current)))

    (defun my-describe-theme () ; Show the current theme
      (interactive)
      (message "%s" (car theme-current)))

   ; Set the next theme (fixed by Chris Webber - tanks)
    (defun my-theme-cycle ()            
      (interactive)
      (setq theme-current (cdr theme-current))
      (if (null theme-current)
      (setq theme-current my-color-themes))
      (funcall (car theme-current))
      (message "%S" (car theme-current)))

    (setq theme-current my-color-themes)
    (setq color-theme-is-global nil) ; Initialization
    (my-theme-set-default)
    (global-set-key [f4] 'my-theme-cycle)

;; ==========================================================================
;; org-mode settings
;; ==========================================================================
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

(setq org-todo-keywords
      '((sequence "TODO(t)" "UNDERGOING(u)" "FIXME(f)" "|" "DONE(d)" "CANCELED(c)")))
;; set up agenda files
;; (setq org-agenda-files (list "~/org/work.org"))

;; ==========================================================================
;; mew settings
;; ==========================================================================
;; vew html mail in mew, but how about picture?
(require 'mew-w3m)
(auto-image-file-mode t)

(autoload 'mew "mew" nil t)
(autoload 'mew-send "mew" nil t)
;; Optional setup (Read Mail menu for Emacs 21):
(if (boundp 'read-mail-command)
	(setq read-mail-command 'mew))
;; Optional setup (e.g. C-xm for sending a message):
(autoload 'mew-user-agent-compose "mew" nil t)
(if (boundp 'mail-user-agent)
	(setq mail-user-agent 'mew-user-agent))
(if (fboundp 'define-mail-user-agent)
	(define-mail-user-agent
	  'mew-user-agent
	  'mew-user-agent-compose
	  'mew-draft-send-message
	  'mew-draft-kill
	  'mew-send-hook))
(setq mew-use-cached-passwd	t)
;; mew-pop-size设置成0时，pop邮件大小没有限制
(setq mew-pop-size 0)
;; 不删除服务器上的邮件
(setq mew-pop-delete nil)

(setq mew-config-alist
	  '(
		(default
		 (pop-server			"pop3.163.com")
		 (pop-port			"110")
		 (name				"liunx")
		 (user				"liunx163")
		 (pop-auth			pass)
		 (mail-domain		   "163.com")
		 (pop-user			  "liunx163@163.com")
		 (smtp-user			 "liunx163@163.com")
		 (smtp-server		   "smtp.163.com")
		 (smtp-auth-list	("PLAIN" "LOGIN" "CRAM-MD5"))
		)))

;; ==========================================================================
;; w3m settings
;; ==========================================================================
;load & init 
(autoload 'w3m "w3m" "interface for w3m on emacs" t)
(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
(autoload 'w3m-search "w3m-search" "Search words using emacs-w3m." t)

;settings
(setq w3m-use-cookies t)
(setq w3m-home-page "http://www.google.com")

(require 'mime-w3m) 
(setq w3m-default-display-inline-image t) 
(setq w3m-default-toggle-inline-images t)

;; ==========================================================================
;; vimpulse settings
;; ==========================================================================
(require 'vimpulse)

;; ==========================================================================
;; vimpulse settings
;; ==========================================================================
(require 'undo-tree)

;; detail see the undo-tree.el comments
(undo-tree-mode)
(global-undo-tree-mode)

;; ==========================================================================
;; emms settings
;; ==========================================================================
(require 'emms-setup)
(emms-standard)
(emms-default-players)
