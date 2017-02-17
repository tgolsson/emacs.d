
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
    (setq
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
          helm-flx-for-helm-locate t
          helm-buffer-file-name nil)))

(use-package helm-swoop
  :ensure t
  :bind (("M-s" . helm-swoop)
         ("C-x M-s" . helm-swoop)))

(use-package helm-bibtex
  :ensure t
  :bind (("C-c 0" . helm-bibtex)
         ("C-c 1" . helm-bibtex-with-local-bibliography))
  :config
  (setq bibtex-completion-additional-search-fields '(keywords)
        helm-bibtex-full-frame nil)
  (helm-delete-action-from-source "Insert citation" helm-source-bibtex)
  (helm-add-action-to-source "Insert citation"
                             'helm-bibtex-insert-citation
                             helm-source-bibtex 0))

(use-package helm-descbinds
  :ensure t
  :bind (("C-h b" . helm-descbinds)))
  
(use-package helm-flycheck
  :ensure t
  :bind (:map flycheck-mode-map ("C-c ! h" . helm-flycheck)))

(use-package helm-describe-modes
  :ensure t)

(use-package helm-fuzzier
  :ensure t
  :config
  (helm-fuzzier-mode 1))

(use-package helm-ls-git
  :ensure t
  :bind ( ("C-x M-g" . helm-ls-git-ls)))

(use-package helm-projectile
  :defer 3 ;; to make sure projectile is loaded and doesn't kill the keybind
  :bind (("C-c p f" . helm-projectile))
  :config (helm-projectile-on))

(use-package helm-mu
  :ensure t
  :if (require 'mu4e nil 'noerror))

(use-package helm-smex
  :ensure t
  :bind (("<remap> <execute-extended-command>" . helm-smex)
         ("M-X". helm-smex-major-mode-commands)))

(use-package helm-w32-launcher
  :ensure t
  :disabled (eq 'system-type 'windows-nt)
  :if (eq 'system-type 'windows-nt)
  :bind (("<f9>" . helm-w32-launcher)))

(provide 'helm-setup)
