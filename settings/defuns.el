(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))






(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input if
linum-mode is disabled"
  (interactive)
  (let ((has-linum linum-mode))
    (unwind-protect
      (progn
        (when (not has-linum) (linum-mode 1))
        (goto-line (read-number "Goto line: ")))
      (if (not has-linum) (linum-mode -1)
        (linum-update (current-buffer))))))


(defun cleanup-buffer ()
    "Perform a bunch of operations on the whitespace content of a buffer.
Including indent-buffer, which should not be called automatically on save."
    (interactive)
    (untabify-buffer)
    (delete-trailing-whitespace)
    (indent-buffer))



(defun transpose-params ()
  "Presumes that params are in the form (p, p, p) or {p, p, p} or [p, p, p]"
  (interactive)
  (let* ((end-of-first (cond
			((looking-at ", ") (point))
			((and (looking-back ",") (looking-at " ")) (- (point) 1))
			((looking-back ", ") (- (point) 2))
			(t (error "Place point between params to transpose."))))
	 (start-of-first (save-excursion
			   (goto-char end-of-first)
			   (move-backward-out-of-param)
			   (point)))
	 (start-of-last (+ end-of-first 2))
	 (end-of-last (save-excursion
			(goto-char start-of-last)
			(move-forward-out-of-param)
			(point))))
    (transpose-regions start-of-first end-of-first start-of-last end-of-last)))

;; shorthand for interactive lambdas
(defmacro λ (&rest body)
  `(lambda ()
     (interactive)
     ,@body))


(defun isearch-forward-use-region ()
  (interactive)
  (when (region-active-p)
    (add-to-history 'search-ring (region-as-string))
    (deactivate-mark))
  (call-interactively 'isearch-forward))

(defun isearch-backward-use-region ()
  (interactive)
  (when (region-active-p)
    (add-to-history 'search-ring (region-as-string))
    (deactivate-mark))
  (call-interactively 'isearch-backward))

(eval-after-load "multiple-cursors"
  '(progn
     (unsupported-cmd isearch-forward-use-region ".")
     (unsupported-cmd isearch-backward-use-region ".")))

(defun check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
        (backward-char 1)
        (if (looking-at "->") t nil)))))

(defun do-yas-expand ()
  (let ((yas/fallback-behavior 'return-nil))
    (yas/expand)))

(defun tab-indent-or-complete ()
  (interactive)
    (if (or (not (or yas-global-mode yas/minor-mode))
            (null (do-yas-expand)))
        (if (check-expansion)
            (company-complete-common)
          (indent-for-tab-command))))

(defun to/kill-other-buffers (&optional kill-special)
  "Kill buffers that do not belong to a `projectile' project.

With prefix argument (`C-u'), also kill the special buffers."
  (interactive "P")
  (let ((bufs (buffer-list (selected-frame))))
    (dolist (buf bufs)
      (with-current-buffer buf
        (let ((buf-name (buffer-name buf)))
          (when (or (null (projectile-project-p))
                    (and kill-special
                         (string-match "^\*"
                                       buf-name)))
            ;; Preserve buffers with names starting with *scratch or
            ;; *Messages
            (unless (string-match "^\\*\\(\\scratch\\|Messages\\)"
                                  buf-name)
              (message "Killing buffer %s" buf-name)
              (kill-buffer buf))))))))


(defun to/close-older-buffers ()
  ;; Run in interactive
  (interactive)
  ;; let bufs -> reverse buffer list (oldest first)
  ;;     numbufs -> number of buffs in buffer-list
  (let ((bufs (reverse (buffer-list (selected-frame))))
        (numbufs (length (buffer-list (selected-frame)))))
    ;; for each buffer
    (dolist (buf bufs)
      ;; if there's more than 10 buffers in list
      (if (> numbufs 10)
          ;; let current buffer be buf
          (with-current-buffer buf
            (progn
              ;; extract the buffer-name for easier use
              (setq buf-name (buffer-name buf))
              ;; if buffer-name begins with star
              (if (string-match "\*" buf-name)
                  ;; reduce remaining buffers by 1
                  (setq numbufs (- numbufs 1))
                ;; else
                (progn
                  ;; if modified, prompt before killing
                  (if (buffer-modified-p buf)
                      (if (y-or-n-p (concat "Kill unsaved buffer " buf-name "?"))
                          ;; if yes
                          (progn
                            ;; kill and reduce ounter
                            (kill-buffer buf)
                            (setq numbufs (- numbufs 1))))
                    ;; else kill directly and reduce counter
                    (progn
                      (kill-buffer)
                      (setq numbufs (- numbufs 1))))))))))))
  
(provide 'defuns)
