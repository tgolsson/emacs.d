(use-package flycheck)
(use-package guru-mode)
(use-package google-contacts)
(use-package speed-type)

;;
;; yas
;;
(use-package yasnippet
  :bind   (:map yas-keymap
                ("<return>" . yas-exit-all-snippets))
  :bind    (:map yas-minor-mode-map
                ("[tab]" . nil)
                ("[?\t]" . nil)
                ("<tab>" . nil))
  :config
  (yas-global-mode 1)
  ;;  (add-to-list 'yas-snippet-dirs "~/.emacs.d/yas") ;; unneeded?
  ;; (yas-reload-all)
  (setq yas-wrap-around-region t))





;;
;; Guide-key
;;
(use-package guide-key
  :config
  (setq guide-key/guide-key-sequence t
        guide-key/idle-delay 1
        guide-key/recursive-key-sequence-flag t
        guide-key/popup-window-position 'bottom)
  (guide-key-mode 1))




(provide 'basic-packages)


