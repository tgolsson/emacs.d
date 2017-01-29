;;
;; magit
;;
(use-package magit
  :defer t
  :bind (("C-x g" . magit-status))
  :bind (:map magit-status-mode-map ( "q" . magit-quit-session))
  :config 
  (setq magit-completing-read-function 'magit-ido-completing-read)
  (add-hook 'magit-status-mode-hook 'magit-filenotify-mode)
  (add-hook 'after-save-hook 'magit-after-save-refresh-status)
  (custom-set-faces
   '(magit-diff-added ((t (:background "black" :foreground "green3"))))
   '(magit-diff-removed ((t (:background "black" :foreground "red3"))))))
(use-package magit-filenotify)

(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

(provide 'setup-magit)
