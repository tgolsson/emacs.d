      ;;                flycheck
      ;;                flycheck-clang-tidy
      ;;                flycheck-golangci-lint
      ;;                flycheck-inline
      ;;                flycheck-irony
      ;;                flycheck-pos-tip
      ;;                flycheck-rust
      ;;                flymake-gjshint
      ;;                flymake-rust
      ;;                flymake-sass

(defun to/adjust-flycheck-automatic-syntax-eagerness ()
    "Adjust how often we check for errors based on if there are any.
This lets us fix any errors as quickly as possible, but in a
clean buffer we're an order of magnitude laxer about checking."
    (setq flycheck-idle-change-delay
	  (if flycheck-current-errors 0.5 30.0)))

(defun flycheck-handle-idle-change ()
    "Handle an expired idle time since the last change.
This is an overwritten version of the original
flycheck-handle-idle-change, which removes the forced deferred.
Timers should only trigger inbetween commands in a single
threaded system and the forced deferred makes errors never show
up before you execute another command."
    (flycheck-clear-idle-change-timer)
    (flycheck-buffer-automatically 'idle-change))


(use-package flycheck
  :commands flycheck-mode
  :init
  (use-package flycheck-clang-tidy
    :ensure t
    :hook (cc-mode . flycheck-clang-tidy-setup)
    :after flycheck)

  (use-package flycheck-pos-tip
    :hook (flycheck-mode-hook . flycheck-pos-tip-mode)
    :config
    (custom-set-variables '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages))
  (make-variable-buffer-local 'flycheck-idle-change-delay)
  :config
  (setq-default flycheck-disabled-checkers '(python-pylint))
  (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled))

  )
  (add-hook 'flycheck-after-syntax-check-hook
	    'to/adjust-flycheck-automatic-syntax-eagerness))

(provide 'flycheck-setup)
