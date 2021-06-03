(setq python-indent-offset 4
      python-environment-virtualenv '("virtualenv" "-p" "python3"
                                      "--system-site-packages" "--quiet")
      flycheck-python-flake8-executable
      "/home/tgolsson/anaconda3/envs/py38/bin/python3.8"
      )


(use-package jedi-core
  :mode "\\.py\\'"
  :init
  (use-package company-jedi
    :commands company-jedi)

  (setq jedi:server-command
      '("~/.emacs.d/.python-environments/default/bin/jediepcserver")
      jedi:use-shortcuts t)
  :config
  (jedi:setup)

  (defun my-python-mode ()
    ;; make these variables local
    (flycheck-mode 1)
    (company-mode 1)
    (make-local-variable 'company-backends)
    (add-to-list 'company-backends '(company-jedi company-yasnippet))
    (flycheck-inline-mode 1)

    (local-set-key (kbd "M-.") 'jedi:goto-definition)
    (local-set-key (kbd "M-,") 'jedi:goto-definition-pop-marker))

  (add-hook 'python-mode-hook 'my-python-mode))
;; ;; CTAGS
;; (global-set-key (kbd "M-.")                  'ctags-search)
