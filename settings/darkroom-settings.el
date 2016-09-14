(require 'darkroom)
(setq darkroom-margins 0.25)
(add-hook 'darkroom-tentative-mode-hook (lambda () (progn 
                                                     (if darkroom-tentative-mode 
                                                         (progn
                                                           (linum-mode 0) (darkroom--set-margins))
                                                       (linum-mode 1)))))

(provide 'darkroom-settings)


