(require 'hlinum)
(global-linum-mode 1)
(linum-mode)
(set 'linum-highlight-in-all-buffersp t)
(hlinum-activate)
(setq linum-format "%4d \u2502 ")
(set-face-bold 'linum-highlight-face t)
(set-face-background 'linum-highlight-face (face-attribute 'default :background))
(set-face-foreground 'linum "#666")
(set-face-foreground 'linum-highlight-face "#999") ;


(provide 'linum-settings)
