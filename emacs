;; Emacs settings
;; Author: 	Lei Liu <liunx163@163.com>
;; License: 	GPL

(defconst liunx-emacs-path "/home/liunx/Work/Emacs/src/liunx-emacs-plugins/" "我的emacs相关配置文件的路径")
(defconst liunx-emacs-my-lisps-path  (concat liunx-emacs-path "my-lisps/") "我自己写的emacs lisp包的路径")
(defconst liunx-emacs-lisps-path     (concat liunx-emacs-path "lisps/") "我下载的emacs lisp包的路径")

;; 把`my-emacs-lisps-path'的所有子目录都加到`load-path'里面
(my-add-subdirs-to-load-path liunx-emacs-lisps-path)
(my-add-subdirs-to-load-path liunx-emacs-my-lisps-path)

;; Mew 配置文件，超级好用的邮件客户端
(require 'mew-settings)

;; 还不能用vimpulse，与emaci冲突
;; (require 'vimpulse)

;; undo-tree
(require 'undo-tree)
;; (global-undo-tree-mode)

(require 'emms-setup)
(emms-standard)
(emms-default-players)

;; tabbar-ruler
;; (setq tabbar-ruler-global-tabbar 't) ; If you want tabbar
;; (setq tabbar-ruler-global-ruler 't) ; If you want a global ruler
;; (setq tabbar-ruler-popup-menu 't) ; If you want a popup menu
;; (setq tabbar-ruler-popup-toolbar 't) ; If you want a popup toolbar
;; (require 'tabbar-ruler)
