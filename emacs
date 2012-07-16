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

(require 'color-theme)
(eval-after-load "color-theme"
		 '(progn
		    (color-theme-initialize)
		    (color-theme-hober)))

;; turn on ido mode easy for file dir search
(require 'ido)
(ido-mode t)

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
