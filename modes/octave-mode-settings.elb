(require 'octave)

(defun my-octave-settings ()
  (abbrev-mode 1)
  (auto-fill-mode 1)
  (font-lock-mode 1)
  (company-mode 1)
  (add-to-list 'company-backends 'company-yasnippet)
)


(defun my-inferior-settings ()
  (abbrev-mode 1)
  (auto-fill-mode 1)
  (font-lock-mode 1)
  (define-key inferior-octave-mode-map [up]
    'comint-previous-input)
  (define-key inferior-octave-mode-map [down]
    'comint-next-input)
  (setq inferior-octave-prompt ">> ")

;;  (add-to-list 'inferior-octave-startup-args "--no-gui")
  )

(add-hook 'octave-mode-hook 'my-octave-settings)
(add-hook 'inferior-octave-mode-hook 'my-inferior-settings)
