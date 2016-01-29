;; active Babel languages

(require 'org-projectile)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (octave . t)
   (sh . t)))
(setq org-agenda-files (list "~/Dropbox/notes/"))
(org-projectile:per-repo)

(add-to-list 'org-capture-templates (org-projectile:project-todo-entry))
(Setq org-projectile:per-repo-filename (concat (projectile-project-name) "-todo.org"))
(setq org-agenda-files (append org-agenda-files (org-projectile:my-todo-files)))
(global-set-key (kbd "C-c M-p") 'org-projectile:project-todo-completing-read)
(global-set-key (kbd "C-c M-c") 'org-capture)

(setq org-confirm-babel-evalute nil)
(setq org-default-notes-file "~/Dropbox/notes/refile.org")
(setq org-use-fast-todo-selection t)
(add-hook 'org-mode-hook (lambda ()
                           (interactive) 
                           (add-hook 'after-save-hook (lambda ()
                           (setq org-agenda-files (list "~/Dropbox/notes/" (org-projectile:todo-files)))
                           (message "Updated agenda list")                           
))))
