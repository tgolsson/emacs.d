;; CTAGS
(global-set-key (kbd "M-.")  'ctags-search)
(global-set-key (kbd "<f7>") 'ctags-create-or-update-tags-table)

(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-z") 'zap-up-to-char)

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "M-j")(lambda ()(interactive)(join-line -1)))

(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)
(global-set-key [remap goto-line] 'goto-line-with-feedback) 

;; Magit
(global-set-key (kbd "C-x g") 'magit-status)

;; Experimental multiple-cursors
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C-S-c C-e") 'mc/edit-ends-of-lines)
(global-set-key (kbd "C-S-c C-a") 'mc/edit-beginnings-of-lines)

;; Mark additional regions matching current region
(global-set-key (kbd "M-ä") 'mc/mark-all-dwim)
(global-set-key (kbd "C-å") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-ä") 'mc/mark-next-like-this)
(global-set-key (kbd "C-S-ä") 'mc/mark-more-like-this-extended)
(global-set-key (kbd "M-å") 'mc/mark-all-in-region)

;; Symbol and word specific mark-more
(global-set-key (kbd "s-ä") 'mc/mark-next-word-like-this)
(global-set-key (kbd "s-å") 'mc/mark-previous-word-like-this)
(global-set-key (kbd "M-s-ä") 'mc/mark-all-words-like-this)
(global-set-key (kbd "s-ä") 'mc/mark-next-symbol-like-this)
(global-set-key (kbd "s-å") 'mc/mark-previous-symbol-like-this)
(global-set-key (kbd "M-s-ä") 'mc/mark-all-symbols-like-this)


;; Perform general cleanup.
(global-set-key (kbd "C-c n") 'cleanup-buffer)
(global-set-key (kbd "C-c C-n") 'cleanup-buffer)
(global-set-key (kbd "C-c C-<return>") 'delete-blank-lines)


;; DMM
(global-set-key (kbd "C-h C-m") 'discover-my-major)
(global-set-key (kbd "C-h M-m") 'discover-my-mode)

;; Transpose
(global-unset-key (kbd "M-t")) ;; which used to be transpose-words
(global-set-key (kbd "M-t l") 'transpose-lines)
(global-set-key (kbd "M-t w") 'transpose-words)
(global-set-key (kbd "M-t s") 'transpose-sexps)
(global-set-key (kbd "M-t p") 'transpose-params)

(global-set-key (kbd "C-c u") 'uncomment-region)
(global-set-key (kbd "C-c c") 'comment-or-uncomment-region)

(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key [tab] 'tab-indent-or-complete)


;;; SMEX
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)


;; projectile

(global-set-key [f8] 'neotree-toggle)
(global-set-key (kbd "C-x M-k") 'to/kill-other-buffers)
(provide 'keybindings)
