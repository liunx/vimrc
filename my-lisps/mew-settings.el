;; ==========================================================================
;; mew settings
;; ==========================================================================
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
(setq mew-pop-size 0)
;;(setq mew-imap-prefix-list '("#mh/" "#mhinbox"))
;;(setq mew-auto-get t)
(setq toolbar-mail-reader 'Mew)
(setq mew-use-cached-passwd t)
(setq mew-passwd-timer-unit 999)
(setq mew-passwd-lifetime 999)
(set-default 'mew-decode-quoted 't)  
(setq mew-prog-pgp "gpg")
(setq mew-pop-delete nil)
(setq mew-config-alist
      '(("default"
	("name"		. "Lei Liu")
	("user"		. "liunx1987")
	("mail-domain"	. "gmail.com")
	("proto"	. "+")
	("pop-ssl"	. t)
	("pop-ssl-port"	. "995")
	("prog-ssl"	. "/usr/bin/mewstunnel")
	("pop-auth"	. pass)
	("pop-user"	. "liunx1987")
	("pop-server"	. "pop.gmail.com")
	("smtp-ssl"	. t)
	("smtp-ssl-port". "465")
	("smtp-auth-list" . ("PLAIN" "LOGIN" "CRAM-MD5"))
	("smtp-user"	. "liunx1987")
	("smtp-server"	. "smtp.gmail.com")
	)
	("IMAP"
	("name"	. "Lei Liu")
	("user"	. "liunx1987")
	("mail-domain" . "gmail.com")
	("proto" . "%")
	("imap-server"	. "imap.gmail.com")
;;	("imap-ssh-server"	. "SSH server address")
	("imap-user"	. "liunx1987@gmail.com")
	("imap-size"	. 0)
	("smtp-ssl"	. t)
	("smtp-ssl-port". "465")
	("smtp-auth-list" . ("PLAIN" "LOGIN" "CRAM-MD5"))
	("smtp-user"	. "liunx1987")
	("smtp-server"	. "smtp.gmail.com")
	("imap-delete" . t)
	("imap-queue-folder" . "%queue") 
	("imap-trash-folder" . "%INBOX.Trash") ;; This must be in concile with your IMAP box setup
	)
 ))
(setq mew-ssl-verify-level 0)
(setq mew-prog-ssl "/usr/bin/mewstunnel")
(defun mew-settings ()
  "Settings for `mew-mode'.")

(eval-after-load "mew"
  `(mew-settings))

(provide 'mew-settings)
