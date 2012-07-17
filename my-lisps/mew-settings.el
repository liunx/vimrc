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
(defun mew-settings ()
  "Settings for `mew-mode'.")

(eval-after-load "mew"
  `(mew-settings))

(provide 'mew-settings)
