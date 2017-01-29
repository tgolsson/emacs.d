(use-package darkroom
  :config
  (add-hook 'darkroom-tentative-mode-hook (lambda () (progn 
                                                       (if darkroom-tentative-mode 
                                                           (progn
                                                             (linum-mode 0)
                                                             (darkroom--set-margins))
                                                         (linum-mode 1)))))
  (setq darkroom-margins 0.25))


(provide 'darkroom-settings)


