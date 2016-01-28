;; active Babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (octave . t)
   (sh . t)))

(setq org-confirm-babel-evalute nil)
(setq org-agenda-files '("~/Dropbox/notes"))
