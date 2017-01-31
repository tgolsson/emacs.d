;; active Babel languages

(require 'ob-core)
(require 'org-projectile)
(require 'org2blog-autoloads)
(require 'calfw)
(require 'calfw-org)
(require 'ob-ditaa)
;; calfw

(setq calendar-week-start-day 1)
(require 'org-bullets)
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
(setq org-pretty-entities-include-sub-superscripts t)
(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "|" "DONE(d)")
        (sequence "BUG(r)" "KNOWNCAUSE(k)" "|" "FIXED(f)"))))
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
 '((octave . t)
   (shell . t)
   (ditaa . t)))

(defun org-mode-settings ()
  (visual-line-mode 1)
  (setq org-list-demote-modify-bullet '(("1." . "-")))
  (org-bullets-mode 1)
  (require 'org-beautify-theme)
  (setq org-pretty-entities 1)
  (if window-system
    (setq org-startup-with-inline-images t)))

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
