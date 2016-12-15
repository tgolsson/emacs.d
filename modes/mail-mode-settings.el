(when (not (eq system-type 'windows-nt))
  (progn
    (require 'mu4e)
    (require 'mu4e-maildirs-extension)
    (require 'mu4e-multi)
    (require 'gnus-dired)
    
    (add-hook 'mail-mode-hook
              (lambda ()
                (font-lock-add-keywords nil
                                        '(("^[ \t]*>[ \t]*>[ \t]*>.*$"
                                           (0 'mail-multiply-quoted-text-face))
                                          ("^[ \t]*>[ \t]*>.*$"
                                           (0 'mail-double-quoted-text-face))))))

    
    (defun to/set-email-msmtp ()
      (setq message-sendmail-extra-arguments
            (list "-a" user-mail-address)))

    (setf global-mu4e-bookmarks
          '( ("flag:unread AND NOT flag:trashed AND (m:/Bahnhof/Inbox OR m:/TomOlsson/Inbox OR m:/Gmail/Inbox)" "Unread messages (Inboxes)" ?u)
             ("date:today..now"                  "Today's messages"     ?t)
             ("flag:flagged"                  "Flagged"     ?f)
             ("date:7d..now"                     "Last 7 days"          ?w)
             ("mime:image/*"                     "Messages with images" ?p))
          )

    (defun my-mu4e-trash-function(msg)
      (if msg
          (cond
           ((mu4e-message-contact-field-matches msg :to "mail@tomolsson.se") "/TomOlsson/Trash")
           ((mu4e-message-contact-field-matches msg :to "contact@tomolsson.se") "/Contact/Trash")
           ((mu4e-message-contact-field-matches msg :to "tom.olsson@bahnhof.se") "/Bahnhof/Inbox/.Trash")
           ((mu4e-message-contact-field-matches msg :to "tom.g.olsson@gmail.com")"/Gmail/[Gmail].Trash")
           ((mu4e-message-contact-field-matches msg :to "no-reply-bb@oru.se") "/Gmail/[Gmail].Trash")
           ((string-match-p (regexp-quote "Bahnhof") (plist-get msg :path)) "/Bahnhof/Inbox/.Trash")
           ((string-match-p (regexp-quote "TomOlsson") (plist-get msg :path)) "/TomOlsson/Trash")
           ((string-match-p (regexp-quote "Contact") (plist-get msg :path)) "/Contact/Trash")
           ((string-match-p (regexp-quote "Gmail") (plist-get msg :path)) "/Gmail/[Gmail].Trash")
           )    
        "/trash"
        )        
      )

    (setq mu4e-maildir-shortcuts    '(("/TomOlsson/Inbox" . ?t)
                                      ("/Gmail/Inbox" . ?g)
                                      ("/Bahnhof/Inbox" . ?b)
                                      ("/Contact/Inbox" . ?c))
          message-kill-buffer-on-exit t
          mu4e-compose-dont-reply-to-self t
          mu4e-compose-in-new-frame 0
          mu4e-compose-keep-self-cc t
          mu4e-compose-signature       "Med vänliga hälsningar\nTom Olsson\n\n0705-988 728"
          gnus-dired-mail-mode 'mu4e-user-agent
          mu4e-confirm-quit nil
          mu4e-headers-date-format "%d/%b/%Y %H:%M" ; date format
          mu4e-html2text-command "w3m -T text/html"
          mu4e-maildir "/home/tgo/Maildir"
          mu4e-update-interval 300
          mu4e-use-fancy-chars t
          mu4e-user-mail-address-list '("tom.olsson@bahnhof.se"
                                        "contact@tomolsson.se"
                                        "mail@tomolsson.se"
                                        "tom.g.olsson@gmail.com")
          user-mail-address "mail@tomolsson.se"
          user-full-name  "Tom Olsson"
          mu4e-drafts-folder  "/TomOlsson/Drafts"
          mu4e-sent-folder  "/TomOlsson/Sent"
          mu4e-trash-folder  'my-mu4e-trash-function
          mu4e-attachment-dir "~/Download"

          
          mu4e-headers-fields '((:date          .  20)    ;; alternatively, use :human-date
                                (:flags         .   6)
                                (:from          .  20)
                                (:thread-subject . 63)
                                (:to . 20)
                                )
          )

    (defun gnus-dired-mail-buffers ()
      "Return a list of active message buffers."
      (let (buffers)
        (save-current-buffer
          (dolist (buffer (buffer-list t))
            (set-buffer buffer)
            (when (and (derived-mode-p 'message-mode)
                       (null message-sent-message-via))
              (push (buffer-name buffer) buffers))))
        (nreverse buffers)))

    (defun my-mu4e-from-address ()
      "Set the From address based on the To address of the
original."
      (let ((msg mu4e-compose-parent-message)) ;; msg is shorter...
        (if msg
            (setq user-mail-address
                  (cond
                   ((mu4e-message-contact-field-matches
                     msg :to "mail@tomolsson.se")
                    "mail@tomolsson.se")
                   ((mu4e-message-contact-field-matches
                     msg :to "contact@tomolsson.se")
                    "contact@tomolsson.se")
                   ((mu4e-message-contact-field-matches
                     msg :to "tom.olsson@bahnhof.se")
                    "tom.olsson@bahnhof.se")
                   ((mu4e-message-contact-field-matches
                     msg :to "tom.g.olsson@gmail.com")
                    "tom.g.olsson@gmail.com")
                   )))))



    ;; (setq mu4e-multi-account-alist
    ;;       '(
    ;;         ("TomOlsson"
    ;;          (user-mail-address . "mail@tomolsson.se")
    ;;          (user-full-name . "Tom Olsson")
    ;;          (mu4e-drafts-folder . "/TomOlsson/Drafts")
    ;;          (mu4e-sent-folder . "/TomOlsson/Sent")
    ;;          (mu4e-trash-folder . my-mu4e-trash-function)
    ;;          )

    ;;         ("Bahnhof"         
    ;;          (user-mail-address . "tom.olsson@bahnhof.se")
    ;;          (user-full-name . "Tom Olsson")
    ;;          (mu4e-drafts-folder . "/Bahnhof/Inbox/.Drafts")
    ;;          (mu4e-sent-folder . "/Bahnhof/Inbox/.Sent")

    ;;          (mu4e-trash-folder . my-mu4e-trash-function)
    ;;          ) 

    ;;         ("Gmail"         
    ;;          (user-mail-address . "tom.g.olsson@gmail.com")
    ;;          (user-full-name . "Tom Olsson")
    ;;          (mu4e-drafts-folder . "/Gmail/Drafts")
    ;;          (mu4e-refile-folder . "/Gmail/Archived")
    ;;          (mu4e-sent-folder . "/Gmail/Sent")
    
    ;;          (mu4e-trash-folder . my-mu4e-trash-function))
    ;;         ))

    
    (setq mu4e-contexts
          `( ,(make-mu4e-context
               :name "Privat"
               :enter-func (lambda () (mu4e-message "Entering Private context"))
               :leave-func (lambda () (mu4e-message "Leaving Private context"))
               ;; we match based on the contact-fields of the message
               :match-func (lambda (msg)
                             (when msg 
                               (mu4e-message-contact-field-matches msg 
                                                                   :to "mail@tomolsson.se")))
               :vars '( ( user-mail-address      . "mail@tomolsson.se"  )
                        ( user-full-name         . "Tom Olsson" )
                        ( mu4e-compose-signature .
                                                 (concat
                                                  "Med vänlig hälsning\n"
                                                  "Tom Olsson\n\n"
                                                  "0705-988 728\n"))))
             ,(make-mu4e-context
               :name "Contact"
               :enter-func (lambda () (mu4e-message "Entering Contact context"))
               ;; we match based on the contact-fields of the message
               :match-func (lambda (msg)
                             (when msg 
                               (mu4e-message-contact-field-matches msg 
                                                                   :to "contact@tomolsson.se")))
               :vars '( ( user-mail-address      . "contact@tomolsson.se"  )
                        ( user-full-name         . "Tom Olsson" )
                        ( mu4e-compose-signature .
                                                 (concat
                                                  "Med vänlig hälsning\n"
                                                  "Tom Olsson\n\n"
                                                  "0705-988 728\n"))))
             ,(make-mu4e-context
               :name "Bahnhof"
               :enter-func (lambda () (mu4e-message "Switch to the Bahnhof context"))
               ;; no leave-func
               ;; we match based on the contact-fields of the message
               :match-func (lambda (msg)
                             (when msg 
                               (mu4e-message-contact-field-matches msg 
                                                                   :to "tom.olsson@bahnhof.se")))
               :vars '( ( user-mail-address       . "tom.olsson@bahnhof.se" )
                        ( user-full-name          . "Tom Olsson" )
                        ( mu4e-compose-signature  .
                                                  (concat
                                                   "Med vänlig hälsning\n"
                                                   "Tom Olsson\n\n"
                                                   "0705-988 728\n"))))
             ,(make-mu4e-context
               :name "Gmail"
               :enter-func (lambda () (mu4e-message "Switch to the Gmail context"))
               ;; no leave-func
               ;; we match based on the maildir of the message; assume all
               ;; cycling-related messages go into the /cycling maildir
               :match-func (lambda (msg)
                             (when msg
                               (mu4e-message-contact-field-matches msg :to "tom.g.olsson@gmail.com")))
               :vars '( ( user-mail-address       . "tom.g.olsson@gmail.com" )
                        ( user-full-name          . "Tom Olsson" )
                        ( mu4e-compose-signature  .
                                                  (concat "Med vänlig hälsning\n"
                                                          "Tom Olsson\n\n"
                                                          "0705-988 728\n"))
                        ( mu4e-bookmarks
                          ("flag:unread AND NOT flag:trashed AND    (m:/Bahnhof/Inbox OR m:/TomOlsson/Inbox OR m:/Gmail/Inbox)" "Unread messages (Inboxes)" ?u))
                        ))))

    )

  (global-set-key (kbd "C-x m") 'mu4e-compose-new)

  (add-hook 'message-send-mail-hook 'to/set-email-msmtp)
  (add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)
  (add-hook 'mu4e-compose-pre-hook 'my-mu4e-from-address)

  (add-hook 'mu4e-view-mode-hook 'smiley-buffer)
  (add-hook 'mu4e-view-mode-hook 'visual-line-mode)

  (defadvice w3m-goto-next-anchor (before go-to-end-of-anchor activate)
    (when (w3m-anchor-sequence)
      (goto-char (next-single-property-change
                  (point) 'w3m-anchor-sequence))))

  (defadvice w3m-goto-previous-anchor (before go-to-end-of-anchor activate)
    (when (w3m-anchor-sequence)
      (goto-char (previous-single-property-change
                  (point) 'w3m-anchor-sequence))))

  
  (define-key mu4e-view-mode-map (kbd "C-i") 'w3m-next-anchor)
  (define-key mu4e-view-mode-map (kbd "M-<tab>") 'w3m-previous-anchor)

  
(mu4e-maildirs-extension)
  )

