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
