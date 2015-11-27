;; IDO-mode

;; Commands
(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR." t)

;; Put definition of name collisions AFTER name
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Save places
(require 'saveplace)
(setq-default save-place t)

(show-paren-mode 1)
(setq-default indent-tabs-mode nil)
(setq x-select-enable-clipboard t
      x-select-enable-primary t
      save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t
      require-final-newline t
      load-prefer-newer t
      ediff-window-setup-function 'ediff-setup-windows-plain
      save-place-file (concat user-emacs-directory "places")
      vc-make-backup-files t
      backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))


(setq echo-keystrokes 0.1)


;; UTF-8 please
(setq locale-coding-system 'utf-8) ; pxretty
(set-terminal-coding-system 'utf-8) ; pretty
(set-keyboard-coding-system 'utf-8) ; pretty
(set-selection-coding-system 'utf-8) ; please
(prefer-coding-system 'utf-8) ; with sugar on top


;; Show active region
(transient-mark-mode 1)
(make-variable-buffer-local 'transient-mark-mode)
(put 'transient-mark-mode 'permanent-local t)
(setq-default transient-mark-mode t)


;; Lines should be 80 characters wide, not 72
(setq fill-column 80)

;; 80 chars is a good width.
(set-default 'fill-column 80)

;; Save a list of recent files visited. (open recent file with C-x f)
(recentf-mode 1)
(setq recentf-max-saved-items 100) ;; just 20 is too recent

;; Save minibuffer history
(savehist-mode 1)
(setq history-length 1000)


;; Easily navigate sillycased words
(global-subword-mode 1)

;; Keep cursor away from edges when scrolling up/down
(require 'smooth-scrolling)


(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; A saner ediff
(setq ediff-diff-options "-w")
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(electric-pair-mode 1)
(provide 'settings)
