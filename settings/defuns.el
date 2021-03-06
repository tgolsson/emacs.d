(eval-when-compile
  (require 'cl))

(defun untabify-buffer ()
  (interactive)
  (untabify 1 (point-max))
  (if (not (eq major-mode 'mew-draft-mode))
      ;; delete-trailing-whitespace does not work in mew-draft-mode.
      (delete-trailing-whitespace)))


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


(defun check-expansion ()
  (save-excursion
    (if (looking-at "\\_>")
        t
      (backward-char 1)
      (if (looking-at "\\.") t
        (backward-char 1)
        (if (looking-at "->") t nil)))))


(defun to/autoinsert-yas-expand()
  "Replace text in yasnippet template."
  (yas/expand-snippet (buffer-string) (point-min) (point-max)))

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


(defun create-scratch-buffer nil
   "create a scratch buffer"
   (interactive)
   (switch-to-buffer (get-buffer-create "*scratch*"))
   (lisp-interaction-mode))

(defun write-region-delete-and-open(start end filename)
  "function takes current region, and writes it to specified file"
  (interactive "r\nFFilename: ")
  (write-region start end filename t)
  (kill-region start end))


(defun random-uuid ()
  "Insert a UUID. This uses a simple hashing of variable data.
Example of a UUID: 1df63142-a513-c850-31a3-535fc3520c3d

Note: this code uses https://en.wikipedia.org/wiki/Md5"
  (interactive)
  (let ((myStr (md5 (format "%s%s%s%s%s%s%s%s%s%s"
                            (user-uid)
                            (emacs-pid)
                            (system-name)
                            (user-full-name)
                            (current-time)
                            (emacs-uptime)
                            (garbage-collect)
                            (buffer-string)
                            (random)
                            (recent-keys)))))

    (insert (format "%s-%s-4%s-%s%s-%s"
                    (substring myStr 0 8)
                    (substring myStr 8 12)
                    (substring myStr 13 16)
                    (format "%x" (+ 8 (random 4)))
                    (substring myStr 17 20)
                    (substring myStr 20 32)))))

(defun* yank-arg ()
  "Yanks the arg at point, abiding by the syntax-table of the current mode"
  (interactive)
  (let (bounds start end)
    (setq bounds (bounds-of-thing-at-point 'symbol))
    (if bounds
        (progn
          (setq start (car bounds))
          (setq end (cdr bounds)))
      (cond
       ((eq (char-syntax (char-after)) ?\()
        (progn
          (setq start (point))
          (forward-sexp)
          (setq end (point))))
       ((eq (char-syntax (char-before)) ?\()
        (progn
          (backward-char)
          (setq start (point))
          (forward-sexp)
          (setq end (point))))
       ((eq (char-syntax (char-before)) ?\))
        (progn
          (setq end (point))
          (backward-sexp)
          (setq start (point))))
       (t
        (progn
          (message "No arg at point to yank!")
          (return-from yank-arg)))))

    (goto-char end)
    (cond ((eq (char-syntax (char-after)) ?\() ; Followed by a sexp
           (progn (forward-sexp)
                  (setq end (point))))
          ((looking-at "[ ,]") ; Whitespace or comma - zap until next symbol
           (while (looking-at "[ ,]")
             (forward-char))
           (setq end (point))))
    (goto-char start)
    (while (looking-back "[[:space:],\n]")
      (backward-char))
    (setq start (point))
    (kill-region start end)))

(defun toggle-maximize-buffer () "Maximize buffer"
  (interactive)
  (if (= 1 (length (window-list)))
      (jump-to-register '_)
    (progn
      (window-configuration-to-register '_)
      (delete-other-windows))))

(defun move-forward-out-of-param ()
  (while (not (looking-at ")\\|, \\| ?}\\| ?\\]"))
    (cond
     ((point-is-in-string-p) (move-point-forward-out-of-string))
     ((looking-at "(\\|{\\|\\[") (forward-list))
     (t (forward-char)))))

(defun move-backward-out-of-param ()
  (while (not (looking-back "(\\|, \\|{ ?\\|\\[ ?"))
    (cond
     ((point-is-in-string-p) (move-point-backward-out-of-string))
     ((looking-back ")\\|}\\|\\]") (backward-list))
     (t (backward-char)))))

(defun current-quotes-char ()
  (nth 3 (syntax-ppss)))

(defalias 'point-is-in-string-p 'current-quotes-char)

(defun to/regen-rusty-tags-projectile ()
  "generate tags files for all rust projects below the dominating .projectile file"
  (interactive)
  (start-process
   "rusty-tags" ;; process-name
   "*rusty-tags*" ;; buffer-name
   "rusty-tags" ;; executable name

   ;; rest = args
   "-o"
   "-s"
   (expand-file-name (locate-dominating-file
                      default-directory
                      ".projectile"))
   ;; generate for emacs
   "emacs"))

(defun toggle-camelcase-underscores ()
  "Toggle between camelcase and underscore notation for the symbol at point."
  (interactive)
  (save-excursion
    (let* ((bounds (bounds-of-thing-at-point 'symbol))
           (start (car bounds))
           (end (cdr bounds))
           (currently-using-underscores-p (progn (goto-char start)
                                                 (re-search-forward "_" end t))))
      (if currently-using-underscores-p
          (progn
            (upcase-initials-region start end)
            (replace-string "_" "" nil start end)
            (downcase-region start (1+ start)))
        (replace-regexp "\\([A-Z]\\)" "_\\1" nil (1+ start) end)
        (downcase-region start (cdr (bounds-of-thing-at-point 'symbol)))))))

(provide 'defuns)
