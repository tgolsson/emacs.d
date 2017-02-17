(defface bug-face
  '((((class color) (min-colors 88) (background light))
     :foreground "red" :weight bold)
    (((class color) (min-colors 88) (background dark))
     :foreground "red"  :weight bold)
    (((class color) (min-colors 16) (background light))
     :foreground "red" :weight bold)
    (((class color) (min-colors 16) (background dark))
     :foreground "red" :weight bold)
    (((class color) (min-colors 8))
     :foreground "red" :weight bold)
    (t :inverse-video t))
  "Basic face for highlighting bug's."
    :group 'fix-me)

(defface fixme-face
  '((((class color) (min-colors 88) (background light))
     :foreground "orange" :weight bold)
    (((class color) (min-colors 88) (background dark))
     :foreground "orange"  :weight bold)
    (((class color) (min-colors 16) (background light))
     :foreground "orange" :weight bold)
    (((class color) (min-colors 16) (background dark))
     :foreground "orange" :weight bold)
    (((class color) (min-colors 8))
     :foreground "orange" :weight bold)
    (t :inverse-video t))
  "Basic face for highlighting fixme's."
    :group 'fix-me)


(defface todo-face
  '((((class color) (min-colors 88) (background light))
     :foreground "green" :weight bold)
    (((class color) (min-colors 88) (background dark))
     :foreground "green"  :weight bold)
    (((class color) (min-colors 16) (background light))
     :foreground "green" :weight bold)
    (((class color) (min-colors 16) (background dark))
     :foreground "green" :weight bold)
    (((class color) (min-colors 8))
     :foreground "green" :weight bold)
    (t :inverse-video t))
  "Basic face for highlighting todo's."
    :group 'fix-me)


(define-minor-mode fixme-mode "Toggles fixme-mode.
Without any argument, this toggles the mode. A positive prefix argument will
  enable the mode, all other will disable it."
  nil
  " FIXME"
  '()
  :group 'fix-me
  (font-lock-add-keywords nil '(("\\<\\(BUG\\|bug\\)\\>" 0 'bug-face t)))
  (font-lock-add-keywords nil '(("\\<\\(TODO\\|todo\\|\\)\\>" 0 'todo-face t)))
  (font-lock-add-keywords nil '(("\\<\\(fixme\\|FIXME\\)\\>" 0 'fixme-face t)))
  )

(add-hook 'prog-mode-hook (lambda () (interactive "") (fixme-mode 1)))

(provide 'fixme-mode)
;; BUG hello
;; TODO add more
;; FIXME this is important
