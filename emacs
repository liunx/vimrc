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
