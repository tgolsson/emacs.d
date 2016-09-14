(require 'ctags)
(require 'yasnippet)
(yas-global-mode 1)
(add-to-list 'yas-snippet-dirs "~/.emacs.d/yas")
(setq yas-wrap-around-region t)
(define-key yas-keymap (kbd "<return>") 'yas-exit-all-snippets)
(require 'magit)
(require 'guide-key)
(setq guide-key/guide-key-sequence '("C-x r" "C-x 4" "C-x v" "C-x 8" "C-x +"))
(guide-key-mode 1)
(setq guide-key/recursive-key-sequence-flag t)
(setq guide-key/popup-window-position 'bottom)

(require 'smex) ; Not needed if you use package.el
(smex-initialize) ; Can be omitted. This might cause a (minimal) delay
                                        ; when Smex is auto-initialized on its first run.

(require 'discover-my-major) 
(require 'guru-mode)
(require 'projectile)
(projectile-global-mode)


(provide 'basic-packages)
