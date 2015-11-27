;; ARDUINO-mode
(require 'cl)

(require 'arduino-mode)
(require 'company-arduino)
(defun my-arduino-mode ()
  ;; enable web mode
  (arduino-mode)
  
  )
(add-hook 'arduino-hook 'my-arduino-mode)

