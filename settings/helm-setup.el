
(use-package helm
  :ensure t
  :bind
  (("C-x C-f" . helm-find-files)
   ("C-x b" . helm-mini)
   ("M-y" . helm-show-kill-ring)
   ("C-h a" . helm-apropos)
   ("C-h i" . helm-info-emacs)
   ("<f6>" . helm-recentf)
   ("C-x r l" . helm-filtered-bookmarks)
   ("C-x c!" . helm-calcul-expression))
  :config
  (define-key helm-map (kbd "<left>") 'helm-previous-source)
  (define-key helm-map (kbd "<right>") 'helm-next-source)
  (setq
   helm-mode-fuzzy-match t
   helm-buffers-fuzzy-matching t
   helm-recentf-fuzzy-match    t
   helm-ff-transformer-show-only-basename t
   helm-move-to-line-cycle-in-source t
   helm-ff-auto-update-initial-value t
   helm-ag-insert-at-point 'symbol)
  (helm-mode t)
  (helm-adaptive-mode t)
  ;; for helm-find-files
  (customize-set-variable 'helm-ff-lynx-style-map t)

  ;; for helm-imenu
  (customize-set-variable 'helm-imenu-lynx-style-map t)

  ;; for semantic
  (customize-set-variable 'helm-semantic-lynx-style-map t)

  ;; for helm-occur
  (customize-set-variable 'helm-occur-use-ioccur-style-keys t)

  ;; for helm-grep
  (customize-set-variable 'helm-grep-use-ioccur-style-keys t)
  (use-package helm-flx
    :config (helm-flx-mode +1)
    (setq helm-flx-for-helm-find-files t
	  helm-flx-for-helm-locate t
	  helm-buffer-file-name nil))
  )

(use-package helm-swoop
  :ensure t
  :bind (("M-s" . helm-swoop)
         ("C-x M-s" . helm-swoop)))


(use-package helm-descbinds
  :ensure t
  :bind (("C-h b" . helm-descbinds)))

(use-package helm-flycheck
  :ensure t
  :bind (:map flycheck-mode-map ("C-c ! h" . helm-flycheck)))

(use-package helm-describe-modes
  :ensure t)

(use-package helm-ls-git
  :ensure t
  :bind ( ("C-x M-g" . helm-ls-git-ls)))

(use-package helm-smex
  :ensure t
  :bind (("<remap> <execute-extended-command>" . helm-smex)
         ("M-X". helm-smex-major-mode-commands)))

(provide 'helm-setup)
