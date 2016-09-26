(require 'hlinum)
(global-linum-mode 1)
(linum-mode)
(set 'linum-highlight-in-all-buffersp t)
(hlinum-activate)
(setq linum-format "%4d  ") ; \u2502
(setq linum-delay t)
(setq linum-eager t)

(provide 'linum-settings)
