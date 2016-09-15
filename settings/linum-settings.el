(require 'hlinum)
(global-linum-mode 1)
(linum-mode)
(set 'linum-highlight-in-all-buffersp t)
(hlinum-activate)
(setq linum-format "%4d \u2502 ")
(set-face-bold 'linum-highlight-face t)
(set-face-background 'linum-highlight-face "#111")
(set-face-foreground 'linum "#666")
(set-face-foreground 'linum-highlight-face "color-196") ;


(provide 'linum-settings)
