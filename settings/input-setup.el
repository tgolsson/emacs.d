;;
;; Let's get modern...
;;
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(define-coding-system-alias 'cp65001 'utf-8)


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
      fill-column 80
      delete-selection-mode t
      confirm-kill-emacs 'y-or-n-p)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
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
   ("M-å" .    mc/mark-all-in-region))
  :config
  (unsupported-cmd isearch-forward-use-region ".")
  (unsupported-cmd isearch-backward-use-region "."))



(use-package flycheck)
(use-package guru-mode)
(use-package speed-type)

;;
;; yas
;;
(use-package yasnippet
  :bind   (:map yas-keymap
                ("<return>" . yas-exit-all-snippets))
  :bind    (:map yas-minor-mode-map
                ("C-i" . nil)
                ("<tab>" . nil))
  :config
  (yas-global-mode 1)
  (setq yas-wrap-around-region t))





;;
;; Guide-key
;;
(use-package guide-key
  :config
  (setq guide-key/guide-key-sequence t
        guide-key/idle-delay 1
        guide-key/recursive-key-sequence-flag t
        guide-key/popup-window-position 'bottom)
  (guide-key-mode 1))


(use-package autoinsert
  :init
  (setq auto-insert-directory (expand-file-name "templates" user-emacs-directory))
  ;; Don't want to be prompted before insertion:
  (setq auto-insert-query nil)
  (add-hook 'find-file-hook 'auto-insert)
  (auto-insert-mode 1)
  (define-auto-insert "\\.el$" ["default-lisp.el" to/autoinsert-yas-expand])
  (define-auto-insert "\\.py$" ["default-python.py" to/autoinsert-yas-expand])
  (define-auto-insert "/sprints/" ["sprint.org" to/autoinsert-yas-expand])
  (define-auto-insert "test_.*.py" ["test.py" to/autoinsert-yas-expand])
   ;; autoinsert C/C++ header
    (define-auto-insert
      (cons "\\.\\([Hh]\\|hh\\|hpp\\)\\'" "My C / C++ header")
      '(nil
    	"// " (file-name-nondirectory buffer-file-name) "\n"
    	"//\n"
        "// Copyright © Tom Olsson\n"
    	"//\n"
    	"// Description:\n"
    	"//\n"
    	(make-string 70 ?/) "\n\n"
        "// last-edit-by: <" (user-full-name) "> \n"
    	"// $Log:$\n"
    	"//\n"
    	(make-string 70 ?/) "\n\n"
        "#pragma once\n\n"
    	))

    ;; auto insert C/C++
    (define-auto-insert
      (cons "\\.\\([Cc]\\|cc\\|cpp\\)\\'" "My C++ implementation")
      '(nil
    	"// " (file-name-nondirectory buffer-file-name) "\n"
    	"//\n"
        "// Copyright © Tom Olsson\n"
    	"// \n"
    	"// Description:\n"
    	"//\n"
    	(make-string 70 ?/) "\n\n"
    	"// last-edit-by: <" (user-full-name) "> \n"
    	"// $Log:$\n"
    	"//\n"
        (make-string 70 ?/) "\n\n"
        "#include \"" (concat (file-name-sans-extension (file-name-nondirectory buffer-file-name)) ".h") "\""
    	)))


(use-package hideshow
  :config
  (define-key hs-minor-mode-map (kbd "C-c h") (lookup-key hs-minor-mode-map (kbd "C-c @")))
  (define-key hs-minor-mode-map (kbd "C-c @") nil))

(bind-key "M-Q" 'delete-trailing-whitespace)

(setq comint-prompt-read-only t)
(defun to/preoutput-turn-buffer-read-only (text)
  (propertize text 'read-only t))
(add-hook 'comint-preoutput-filter-functions
          'to/preoutput-turn-buffer-read-only)

(use-package expand-region
  :ensure t
  :bind ("C-@" . er/expand-region))


(provide 'input-setup)
