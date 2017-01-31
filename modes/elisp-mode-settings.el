(defun byte-compile-current-buffer ()
  "`byte-compile' current buffer if it's emacs-lisp-mode and compiled file exists."
    (interactive)
    (when (and (eq major-mode 'emacs-lisp-mode)
               (file-exists-p
                (byte-compile-dest-file buffer-file-name)))
      (byte-compile-file buffer-file-name)))

(use-package lisp-mode
  :ensure nil
  :mode ("\\.el\\'" . emacs-lisp-mode)
  :bind (:map emacs-lisp-mode-map ("\r" . reindent-then-newline-and-indent))
  :config
  (add-hook 'emacs-lisp-mode-hook
            '(lambda ()
               (abbrev-mode 1)
               (auto-fill-mode 1)
               (font-lock-mode 1)
               (company-mode 1)
               (eldoc-mode 1)
               (flyspell-prog-mode)
               (make-local-variable 'company-backends)
               (add-to-list 'company-backends '(company-elisp company-yasnippet))
               (add-hook 'after-save-hook 'byte-compile-current-buffer nil t))))
