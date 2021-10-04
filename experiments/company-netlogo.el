;; (use-package netlogo-mode
;;   :load-path "packages"
;;   :mode "\\.logo\\'"
;;   :config
;;   (setq company-netlogo-keywords
;;         (sort
;;          (mapcar
;;           (lambda (x) (if (stringp x)
;;                           x
;;                         (number-to-string x)))
;;           (append
;;            (list)
;;            netlogo-functions
;;            netlogo-keywords-breeds-left
;;            netlogo-keywords-breeds-right
;;            netlogo-logic-keywords netlogo-types)) 'string<))

;;   (defun company-netlogo (command &optional arg &rest ignored)
;;     (interactive (list 'interactive))
;;     (case command
;;       (interactive (progn
;;                      (company-begin-backend 'company-netlogo)))
;;       (prefix (company-grab-symbol))
;;       (candidates (delq nil
;;                         (mapcar (lambda (x) (and (string-prefix-p arg x) x)) company-netlogo-keywords))))))
