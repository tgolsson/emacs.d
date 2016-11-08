;; active Babel languages

(require 'org-projectile)
(require 'org2blog-autoloads)
(require 'calfw)
(require 'calfw-org)

;; calfw

(setq calendar-week-start-day 1)

;; Unicode characters
(setq cfw:fchar-junction ?╋
      cfw:fchar-vertical-line ?┃
      cfw:fchar-horizontal-line ?━
      cfw:fchar-left-junction ?┣
      cfw:fchar-right-junction ?┫
      cfw:fchar-top-junction ?┯
      cfw:fchar-top-left-corner ?┏
      cfw:fchar-top-right-corner ?┓)


;; General org
(setq org-latex-pdf-process (list "latexmk -f -lualatex %f"))
(setq org-directory"~/Dropbox/notes")
(setq org-use-fast-todo-selection t)
(setq org-list-allow-alphabetical t)
(setq org-default-notes-file "~/Dropbox/notes/refile.org")
(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "|" "DONE(d)")
        (sequence "BUG(r)" "KNOWNCAUSE(k)" "|" "FIXED(f)"))))
(org-toggle-pretty-entities)
;; Org-agenda
(setq org-agenda-files (list "~/Dropbox/notes"))
;; Org-projectile

(setq org-capture-templates
      (quote (
              ("m" "* Current region" entry (file org-default-notes-file)
               "* MEMO %? \n%^G\n%i\n" :kill-buffer)
              )
             ))

(add-to-list 'org-capture-templates
             (org-projectile:project-todo-entry "b"  "* BUG %f %? \n%^G\n%a\n"
                                                "Project Bug"))
(add-to-list 'org-capture-templates
             (org-projectile:project-todo-entry "t" "* TODO %f %? \n%^G\n%a\n" "Project Todo"))

(setq org-projectile:projects-file
      "/home/tgo/Dropbox/notes/todo-projects.org")

;; Org2blog
(setq org2blog/wp-buffer-template
      "-----------------------
#+TITLE: %s
#+DATE: %s
#+CATEGORY:
#+TAGS: 
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
(defun org-mode-settings ()
  (visual-line-mode 1)
  (setq org-list-demote-modify-bullet '(("1." . "-")))
  )

;; org-gcal
(require 'org-gcal)

(setq org-latex-classes t)
(add-to-list 'org-latex-classes
      '("org-article"
         "\\documentclass[twocolumn]{article}
         [NO-DEFAULT-PACKAGES]
         [PACKAGES]
         [EXTRA]"
         ("\\section{%s}" . "\\section*{%s}")
         ("\\subsection{%s}" . "\\subsection*{%s}")
         ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
         ("\\paragraph{%s}" . "\\paragraph*{%s}")
         ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(add-hook 'org-mode-hook 'org-mode-settings)
(global-set-key (kbd "C-c M-c") 'org-capture)


