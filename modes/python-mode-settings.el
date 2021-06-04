(setq python-indent-offset 4
      python-environment-virtualenv '("virtualenv" "-p" "python3"
                                      "--system-site-packages" "--quiet")
      flycheck-python-flake8-executable
      "/home/tgolsson/anaconda3/envs/py38/bin/python3.8"
      )


(use-package python-mode
  :mode "\\.py\\'"
  :init
  (use-package conda
    :commands conda-env-activate
    :init
    (setq conda-env-home-directory "/home/tgolsson/anaconda3"
          conda-anaconda-home "/home/tgolsson/anaconda3")
    :config
    (conda-env-activate 'getenv "CONDA_DEFAULT_ENV")
    (conda-env-autoactivate-mode t))

  (use-package jedi-core
    :hook (python-mode . jedi-mode)
    :init
    (use-package company-jedi
      :commands company-jedi)

    (setq jedi:server-command
          '("~/.emacs.d/.python-environments/default/bin/jediepcserver")
          jedi:use-shortcuts t)
    :config
    (jedi:setup))

  (defun my-python-mode ()
    ;; make these variables local
    (flycheck-mode 1)
    (company-mode 1)
    (make-local-variable 'company-backends)
    (add-to-list 'company-backends '(company-jedi company-yasnippet))
    (flycheck-inline-mode 1)

    (local-set-key (kbd "M-.") 'jedi:goto-definition)
    (local-set-key (kbd "M-,") 'jedi:goto-definition-pop-marker))

  (add-hook 'python-mode-hook 'my-python-mode)
  )

;; ;; CTAGS
;; (global-set-key (kbd "M-.")                  'ctags-search)
