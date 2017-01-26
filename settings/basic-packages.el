; (require 'ctags) //windows fix
(require 'flycheck)
;; yas
(require 'yasnippet)
(yas-global-mode 1)
(add-to-list 'yas-snippet-dirs "~/.emacs.d/yas")
(yas-reload-all)
(setq yas-wrap-around-region t)
(define-key yas-keymap (kbd "<return>") 'yas-exit-all-snippets)

;; magit
(require 'magit)
(require 'magit-filenotify)
(setq magit-completing-read-function 'magit-ido-completing-read)
(add-hook 'magit-status-mode-hook 'magit-filenotify-mode)
(add-hook 'after-save-hook 'magit-after-save-refresh-status)
(custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(magit-diff-added ((t (:background "black" :foreground "green3"))))
   '(magit-diff-removed ((t (:background "black" :foreground "red3")))))

(require 'guide-key)
(setq guide-key/guide-key-sequence t)
(setq guide-key/idle-delay 1)

(guide-key-mode 1)
(setq guide-key/recursive-key-sequence-flag t)
(setq guide-key/popup-window-position 'bottom)

(require 'ido)
;; ido-mode
(ido-mode t)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)

(require 'smex) ; Not needed if you use package.el
(smex-initialize) ; Can be omitted. This might cause a (minimal) delay
                                        ; when Smex is auto-initialized on its first run.

(require 'discover-my-major) 
(require 'guru-mode)

(require 'google-contacts)
(require 'speed-type)
(provide 'basic-packages)


