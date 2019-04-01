;; CTAGS
(global-set-key (kbd "M-.")                  'ctags-search)
(global-set-key (kbd "<f7>")                 'ctags-create-or-update-tags-table)

(global-set-key (kbd "M-/")                  'hippie-expand)
(global-set-key (kbd "C-x C-b")              'ibuffer)
(global-set-key (kbd "M-z")                  'zap-up-to-char)

(global-set-key (kbd "C-s")                  'isearch-forward-regexp)
(global-set-key (kbd "C-r")                  'isearch-backward-regexp)
(global-set-key (kbd "C-M-s")                'isearch-forward)
(global-set-key (kbd "C-M-r")                'isearch-backward)
(global-set-key (kbd "M-j")                   (lambda ()(interactive)(join-line -1)))

(global-set-key (kbd "C-x C-r")              'rename-current-buffer-file)
(global-set-key [remap goto-line]            'goto-line-with-feedback) 

;; Perform general cleanup.
(global-set-key (kbd "C-c n")                'cleanup-buffer)
(global-set-key (kbd "C-c C-n")              'cleanup-buffer)
(global-set-key (kbd "C-c C-<return>")       'delete-blank-lines)

;; DMM
(global-set-key (kbd "C-h C-m")              'discover-my-major)
(global-set-key (kbd "C-h M-m")              'discover-my-mode)

;; Transpose
(global-unset-key (kbd "M-t")) ;; which used to be transpose-words
(global-set-key (kbd "M-t l")                'transpose-lines)
(global-set-key (kbd "M-t w")                'transpose-words)
(global-set-key (kbd "M-t s")                'transpose-sexps)
(global-set-key (kbd "M-t p")                'transpose-params)

(global-set-key (kbd "C-c u")                'uncomment-region)
(global-set-key (kbd "C-c c")                'comment-or-uncomment-region)

(global-set-key (kbd "M-p")                  'backward-paragraph)
(global-set-key (kbd "M-n")                  'forward-paragraph)
;;(global-set-key [tab]                      'tab-indent-or-complete)
;(define-key company-active-map [tab]        'tab-indent-or-complete)

(global-set-key (kbd "M-i") 'helm-etags-select)
;; programming mode
(define-key prog-mode-map (kbd "M-W") 'yank-arg)
;; company

(define-key company-active-map (kbd "TAB")   'tab-indent-or-complete)
(define-key company-active-map (kbd "<tab>") 'tab-indent-or-complete)

(provide 'keybindings)
