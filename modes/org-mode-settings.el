;; active Babel languages

(require 'org-projectile)
(require 'org2blog-autoloads)

;; General org
(setq org-directory"~/Dropbox/notes")
(setq org-use-fast-todo-selection t)
(setq org-list-allow-alphabetical t)
(setq org-default-notes-file "~/Dropbox/notes/refile.org")
(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "|" "DONE(d)")
        (sequence "BUG(r)" "KNOWNCAUSE(k)" "|" "FIXED(f)"))))

;; Org-agenda
(setq org-agenda-files (list "~/Dropbox/notes"))
;; Org-projectile
(add-to-list 'org-capture-templates
             (org-projectile:project-todo-entry "t" "* TODO %f %? \n%^G\n%a\n" "Project Todo"))
(add-to-list 'org-capture-templates
             (org-projectile:project-todo-entry "b"  "* BUG %f %? \n%^G\n%a\n" "Project Bug"))
(setq org-projectile:projects-file
      "/home/tgo/Dropbox/notes/todo-projects.org")

;; Org2blog
(setq org2blog/wp-buffer-template
      "-----------------------
#+TITLE: %s
#+DATE: %s
#+CATEGORY: 
-----------------------\n")
(defun my-format-function (format-string)
  (format format-string
          org2blog/wp-default-title
          (format-time-string "%d-%m-%Y" (current-time))))
(setq org2blog/wp-buffer-format-function 'my-format-function)

(setq org2blog/wp-use-sourcecode-shortcode t)
(setq org2blog/wp-blog-alist
      '(("tomolsson.se"
         :url "http://www.tomolsson.se/xmlrpc.php"
         :username "tgolsson"
         :default-title "New post"
         :default-categories ("Programming" "C++")
                  :tags-as-categories nil)))

;; org-babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (octave . t)
   (sh . t)))
(setq org-confirm-babel-evalute nil)

 

(global-set-key (kbd "C-c M-c") 'org-capture)
