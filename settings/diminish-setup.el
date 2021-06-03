;;
;; Diminish
;;
(use-package diminish
  :demand t
  :defer 10
  :config
  (diminish 'auto-fill-function)
  (eval-after-load "minimap" '(diminish 'minimap-mode))
  (eval-after-load "guide-key" '(diminish 'guide-key-mode))
  (eval-after-load "abbrev" '(diminish 'abbrev-mode))
  (eval-after-load "yasnippet" '(diminish 'yas-minor-mode))
  (eval-after-load "auto-revert" '(diminish 'auto-revert-mode))
  (eval-after-load "flyspell" '(diminish 'flyspell-mode))
  (eval-after-load "flycheck" '(diminish 'flycheck-mode))
  (eval-after-load "eldoc" '(diminish 'eldoc-mode))
  (eval-after-load "subword" '(diminish 'subword-mode))
  (eval-after-load "projectile" '(diminish 'projectile-mode))
  (eval-after-load "rainbow-mode" '(diminish 'rainbow-mode))
  (eval-after-load "fixme-mode" '(diminish 'fixme-mode))
  (eval-after-load "fira-code-mode" '(diminish 'fira-code-mode)))

(provide 'diminish-setup)
