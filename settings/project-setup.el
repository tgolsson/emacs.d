(use-package projectile
  :ensure t
  :demand t  
  ;; :bind (("C-x M-k" . to/kill-other-buffers)
  ;;        ("C-c p a" . projectile-find-other-file))

  :init

  :config
    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
    (projectile-global-mode 1)
    (helm-projectile-on)
  (setq projectile-switch-project-action 'magit-status
        projectile-globally-ignored-directories (append
                                                 '("*__pycache__*" "*.egg-info")
                                                 projectile-globally-ignored-directories)
        projectile-globally-ignored-file-suffixes (append
                                                   '(".pyc")
                                                   projectile-globally-ignored-file-suffixes)


        projectile-indexing-method 'alien
        projectile-enable-caching 't
        projectile-git-command "fd . -0"))

;; (use-package helm-projectile
;;   :ensure t
;;   :demand t
;;   :config
;;   (progn
;;     (setq projectile-completion-system 'helm)
;;     (helm-projectile-on)
;;     )
;;   )

(use-package ibuffer-projectile)

(provide 'project-setup)
