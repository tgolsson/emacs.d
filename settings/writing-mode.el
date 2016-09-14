(define-minor-mode writing-mode
  "Toggle writing mode.

writing-mode is a combined mode of darkroom and draft-mode, and settings,
intended to make writing a lot faster. "
  :init-value nil
  :lighter " Writer"
  :keymap (let ((writer-map (make-sparse-keymap)))
            (define-key writer-map (kbd "C-c d") '(lambda () (interactive) (delete-char 1 )))
            (define-key writer-map (kbd "C-d") nil) writer-map)
  :group 'writing-mode
  (progn (if (bound-and-true-p writing-mode)
             (progn (draft-mode 1) (darkroom-tentative-mode 1))
           (progn (draft-mode 0 ) (darkroom-tentative-mode 0))))
  )
