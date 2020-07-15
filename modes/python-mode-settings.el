(require 'jedi-core)
(require 'company-jedi)                          ; load company mode html backend
(require 'python-django)

(defun my-python-mode ()
  ;; make these variables local
  (setq python-indent-offset 4)
  (jedi:setup)
  (flycheck-mode 1)

  (company-mode 1)
  (make-local-variable 'company-backends)
  (add-to-list 'company-backends '(company-jedi company-yasnippet))
  (flycheck-inline-mode 1)
  (setq jedi:server-command '("~/.emacs.d/.python-environments/default/bin/jediepcserver")
        python-environment-virtualenv '("virtualenv" "-p" "python3"
                                        "--system-site-packages" "--quiet")
        python-django-qmgmt-runserver-default-bindaddr "0.0.0.0:8000"
        jedi:use-shortcuts t)
  (local-set-key (kbd "M-.") 'jedi:goto-definition)
  (local-set-key (kbd "M-,") 'jedi:goto-definition-pop-marker))

(add-hook 'python-mode-hook 'my-python-mode)

;; ;; CTAGS
;; (global-set-key (kbd "M-.")                  'ctags-search)
