;; Emacs settings
;; Author: 	Lei Liu <liunx163@163.com>
;; License: 	GPL

(defconst liunx-emacs-path "/home/liunx/Work/Emacs/src/liunx-emacs-plugins/" "我的emacs相关配置文件的路径")
(defconst liunx-emacs-my-lisps-path  (concat liunx-emacs-path "my-lisps/") "我自己写的emacs lisp包的路径")
(defconst liunx-emacs-lisps-path     (concat liunx-emacs-path "lisps/") "我下载的emacs lisp包的路径")

;; 把`my-emacs-lisps-path'的所有子目录都加到`load-path'里面
(my-add-subdirs-to-load-path liunx-emacs-lisps-path)
(my-add-subdirs-to-load-path liunx-emacs-my-lisps-path)

;; 设置备份文件
(setq backup-directory-alist `(("." . "~/.saves")))
(setq backup-by-copying t)

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

;; author: pluskid
;; 调用 stardict 的命令行程序 sdcv 来查辞典
;; 如果选中了 region 就查询 region 的内容，否则查询当前光标所在的单词
;; 查询结果在一个叫做 *sdcv* 的 buffer 里面显示出来，在这个 buffer 里面
;; 按 q 可以把这个 buffer 放到 buffer 列表末尾，按 d 可以查询单词
(global-set-key (kbd "C-c d") 'kid-sdcv-to-buffer)
(defun kid-sdcv-to-buffer ()
  (interactive)
  (let ((word (if mark-active
                  (buffer-substring-no-properties (region-beginning) (region-end))
                  (current-word nil t))))
    (setq word (read-string (format "Search the dictionary for (default %s): " word)
                            nil nil word))
    (set-buffer (get-buffer-create "*sdcv*"))
    (buffer-disable-undo)
    (erase-buffer)
    (let ((process (start-process-shell-command "sdcv" "*sdcv*" "sdcv" "-n" word)))
      (set-process-sentinel
       process
       (lambda (process signal)
         (when (memq (process-status process) '(exit signal))
           (unless (string= (buffer-name) "*sdcv*")
             (setq kid-sdcv-window-configuration (current-window-configuration))
             (switch-to-buffer-other-window "*sdcv*")
             (local-set-key (kbd "d") 'kid-sdcv-to-buffer)
             (local-set-key (kbd "q") (lambda ()
                                        (interactive)
                                        (bury-buffer)
                                        (unless (null (cdr (window-list))) ; only one window
                                          (delete-window)))))
           (goto-char (point-min))))))))

;; 动态地放大缩小字体，代码来自[http://sachachua.com/blog/2006/09/emacs-changing-the-font-size-on-the-fly/]
(defun sacha/increase-font-size ()
  (interactive)
  (set-face-attribute 'default
                      nil
                      :height
                      (ceiling (* 1.10
                                  (face-attribute 'default :height)))))
(defun sacha/decrease-font-size ()
  (interactive)
  (set-face-attribute 'default
                      nil
                      :height
                      (floor (* 0.9
                                  (face-attribute 'default :height)))))
(global-set-key (kbd "C-+") 'sacha/increase-font-size)
(global-set-key (kbd "C--") 'sacha/decrease-font-size)

;; add diretree like NERDTREE 
(require 'dirtree)
