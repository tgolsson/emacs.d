;; ARDUINO-mode
(require 'cl)

(require 'arduino-mode)
(require 'company-arduino)
(defun my-arduino-mode ()
  ;; enable web mode
  (yas-activate-extra-mode 'c++-mode)
  
  )
(add-hook 'arduino-mode-hook 'my-arduino-mode)

