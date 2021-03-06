;; ============================================================
;; Aaron Bedra's Emacs 24 Configuration
;; ============================================================
;; User details
(setq user-full-name "Lei Liu")
(setq user-mail-address "liunx163@163.com")

;; Environment
(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
(require 'cl)

;; show line number
(global-linum-mode t)

;; Package Management
(load "package")
(package-initialize)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(setq package-archive-enable-alist '(("melpa" deft magit)))

;; Define default packages
(defvar abedra/packages '(ac-slime
                          auto-complete
                          autopair
                          clojure-mode
                          clojure-test-mode
                          coffee-mode
                          deft
                          gist
                          go-mode
                          haml-mode
                          haskell-mode
                          htmlize
                          magit
                          markdown-mode
                          marmalade
                          nrepl
                          o-blog
                          org
                          paredit
                          puppet-mode
                          restclient
                          rvm
                          smex
			  session
                          sml-mode
                          bbdb
                          org-mime
			  tabbar-ruler
                          undo-tree
			  w3m
			  mew
                          yaml-mode)
  "Default packages")
;; Install default packages
(defun abedra/packages-installed-p ()
  (loop for pkg in abedra/packages
        when (not (package-installed-p pkg)) do (return nil)
        finally (return t)))

(unless (abedra/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg abedra/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

;; Start-up options

;; Splash Screen
(setq inhibit-splash-screen t
      initial-scratch-message nil)

(when (locate-library "clojure-mode")
  (setq initial-major-mode 'clojure-mode))

;; Scroll bar, Tool bar, Menu bar
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

;; Marking text
(delete-selection-mode t)
(transient-mark-mode t)
(setq x-select-enable-clipboard t)

;; Display Settings
(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b"))))

;; Indentation
(setq tab-width 4
      indent-tabs-mode nil)

;; Backup files
(setq make-backup-files nil)

;; Yes and No
(defalias 'yes-or-no-p 'y-or-n-p)

;; Key bindings
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; Misc
(setq echo-keystrokes 0.1
      use-dialog-box nil
      visible-bell t)
(show-paren-mode t)

;; ============================================================
;; Org mode
;; ============================================================
(add-to-list 'load-path (expand-file-name "~/Work/Emacs/src/org-mode/lisp"))
;; we also need the contrib part of org-mode
(add-to-list 'load-path (expand-file-name "~/Work/Emacs/src/org-mode/contrib/lisp"))
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))
(require 'org)

;;
;; Standard key bindings
;;
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;;
;; agenda files
;;
(setq org-agenda-files (quote ("~/git/org"
                               "~/git/org/client1"
                               "~/git/org/client2")))

;;
;; load org-mode 
;;
(add-to-list 'load-path (expand-file-name "~/.emacs.d"))
(load "org-mode")

;;
;; 生成中文PDF文件的配置
;;
;(load "my-org-settings")

;; ======================= END ================================
;; ido
(ido-mode t)
(setq ido-enable-flex-matching t
      ido-use-virtual-buffers t)

;; Turn on column numbers
(setq column-number-mode t)

;; Temporary file management
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; autopair-mode
(require 'autopair)

;; session
(require 'session)
(add-hook 'after-init-hook 'session-initialize)

;; tabbar-ruler
(require 'tabbar-ruler)
;;(setq tabbar-ruler-global-tabbar t) ; If you want tabbar
;;(setq tabbar-ruler-global-ruler t) ; if you want a global ruler

;; undo-tree
(require 'undo-tree)
(global-undo-tree-mode)

;; w3m
(require 'w3m)
(require 'w3m-search)
(setq browse-url-browser-function 'w3m-browse-url)
(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
;; optional keyboard short-cut
(global-set-key "\C-xm" 'browse-url-at-point)
(define-key w3m-mode-map [mouse-2] 'w3m-mouse-view-this-url-new-session)
(standard-display-ascii ?\255 [?+])
(standard-display-ascii ?\227 [?-])
(standard-display-ascii ?\222 [?'])
(setq w3m-use-form t)
(setq w3m-tab-width 8)
(setq w3m-use-cookies t)
(setq w3m-use-toolbar t)
(setq w3m-use-mule-ucs t)
(setq w3m-fill-column 120)
(setq w3m-default-display-inline-image t)
(setq w3m-default-toggle-inline-images t)
(setq w3m-home-page "http://cn.bing.com")
(setq browse-url-browser-function 'w3m-browse-url)
(setq w3m-view-this-url-new-session-in-background t)
(setq w3m-search-default-engine "Bingo")
(add-to-list 'w3m-search-engine-alist '("Bingo" "http://cn.bing.com/search?q=%s"))
(defadvice w3m-search (after change-default activate)
	   (let ((engine (nth 1 minibuffer-history)))
	     (when (assoc engine w3m-search-engine-alist)
	       (setq w3m-search-default-engine engine))))

(require 'emamux)
(custom-set-variables
 '(emamux:completing-read-type 'helm))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(column-number-mode t)
 '(custom-enabled-themes (quote (wombat)))
 '(default-input-method "chinese-py-punct")
 '(display-time-mode t)
 '(font-use-system-font t)
 '(fringe-mode (quote (nil . 0)) nil (fringe))
 '(menu-bar-mode nil)
 '(org-agenda-files (quote ("~/Work/Broadcom/doc/商务领航工作任务.org" "/home/liunx/git/org/diary.org" "/home/liunx/git/org/refile.org" "/home/liunx/git/org/test_refile.org" "/home/liunx/git/org/todo.org")))
 '(session-use-package t nil (session))
 '(show-paren-mode t)
 '(size-indication-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Droid Sans Fallback" :foundry "unknown" :slant normal :weight normal :height 120 :width normal))))
 '(org-mode-line-clock ((t (:foreground "red" :box (:line-width -1 :style released-button)))) t))
(put 'downcase-region 'disabled nil)
