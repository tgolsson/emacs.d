
(use-package helm
  :bind (("C-x C-f" . helm-find-files)
         ("C-x b" . helm-mini)
         ("M-y" . helm-show-kill-ring)
         ("C-h a" . helm-apropos)
         ("C-h i" . helm-info-emacs)
         ("<f6>" . helm-recentf)
         ("C-x r l" . helm-filtered-bookmarks)
         ("C-x c!" . helm-calul-expression))

  :config (setq
           helm-mode-fuzzy-match t
           helm-buffers-fuzzy-matching t
           helm-recentf-fuzzy-match    t
           helm-ff-transformer-show-only-basename t
           helm-move-to-line-cycle-in-source t
           helm-ff-auto-update-initial-value t)
  (helm-mode t)
  (helm-adaptive-mode t)
  (use-package helm-flx
    :config (helm-flx-mode +1)
    (setq helm-flx-for-helm-find-files t
          helm-flx-for-helm-locate t)))

(use-package helm-swoop
  :bind (("M-s" . helm-swoop)
         ("C-x M-s" . helm-swoop)))

(use-package helm-descbinds
  :bind (("C-h b" . helm-descbinds)))


  
(use-package helm-flycheck
  :bind (:map flycheck-mode-map ("C-c ! h" . helm-flycheck)))

(use-package helm-describe-modes)

(use-package helm-fuzzier
  :config
  (helm-fuzzier-mode 1))

(use-package helm-ls-git
  :bind ( ("C-x C-g" . helm-ls-git-ls)))

(use-package helm-projectile
  :defer 3 ;; to make sure projectile is loaded and doesn't kill the keybind
  :bind (("C-c p f" . helm-projectile))
  :config (helm-projectile-on))

(use-package helm-mu
  :if (require 'mu4e nil 'noerror))

(use-package helm-smex
  :bind (("<remap> <execute-extended-command>" . helm-smex)
         ("M-X". helm-smex-major-mode-commands)))

(use-package helm-w32-launcher
  :if (eq 'system-type 'windows-nt)
  :bind (("<f9>" . helm-w32-launcher)))

(provide 'helm-setup)
