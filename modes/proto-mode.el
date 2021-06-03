
  (defconst my-protobuf-style
    '((c-basic-offset . 4)
      (indent-tabs-mode . nil)))

(use-package protobuf-mode
  :mode "\\.proto\\'"
  :init
  (add-hook 'protobuf-mode-hook (lambda () (interactive "")
                                  (company-mode 0)
                                  (irony-mode 0)
                                  (c-add-style "my-style" my-protobuf-style t))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; proto-mode.el ends here
