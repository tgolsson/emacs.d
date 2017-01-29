;;
;; Let's get modern...
;; 
(setq locale-coding-system 'utf-8) 
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8) 
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)


;;
;; Transient mark mode
;;
(transient-mark-mode 1)
(make-variable-buffer-local 'transient-mark-mode)
(put 'transient-mark-mode 'permanent-local t)
(setq-default transient-mark-mode t)

;;
;; General input modes
;;
(global-subword-mode 1)
(electric-pair-mode 1)


;;
;; General input setqs
;;
(setq echo-keystrokes 0.1
      x-select-enable-clipboard t
      x-select-enable-primary t
      save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t
      require-final-newline t
      fill-column 80)

;;
;; General input setqs
;;
(setq-default indent-tabs-mode nil)
(setq-default fill-column 80)


;;
;; Multi-cursors
;;
(use-package multiple-cursors
  :bind  
  (("M-ö" .    mc/edit-lines)
   ("C-M-ö" .  mc/edit-ends-of-lines)
   ("M-Ö" .    mc/edit-beginnings-of-lines)
   ("M-ä" .    mc/mark-all-dwim)
   ("C-<" .    mc/mark-previous-like-this)
   ("C->" .    mc/mark-next-like-this)
   ("C-S-ä" .  mc/mark-more-like-this-extended)
   ("M-å" .    mc/mark-all-in-region)))



(provide 'input)
