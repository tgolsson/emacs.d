(defun byte-compile-current-buffer ()
    "`byte-compile' current buffer if it's emacs-lisp-mode and compiled file
exists."
    (interactive)
    (when (and (eq major-mode 'emacs-lisp-mode)
                            (file-exists-p (byte-compile-dest-file
                                            buffer-file-name)))
      (byte-compile-file buffer-file-name)))

(defun my-elisp-settings ()
  (abbrev-mode 1)
  (auto-fill-mode 1)
  (font-lock-mode 1)
  (company-mode 1)
  (make-local-variable 'company-backends)
  (add-to-list 'company-backends '(company-elisp company-yasnippet))

  ;; Use spaces, not tabs.
  (setq indent-tabs-mode nil)

  (define-key emacs-lisp-mode-map
    "\r" 'reindent-then-newline-and-indent))

(
 add-hook 'emacs-lisp-mode-hook (lambda () (add-hook 'after-save-hook
                                                     'byte-compile-current-buffer nil t)))
(add-hook 'emacs-lisp-mode-hook 'my-elisp-settings)
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'flyspell-prog-mode) ;; Requires Ispell

