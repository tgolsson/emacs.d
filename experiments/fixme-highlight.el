

(defface fixme-face
  '((((class color) (min-colors 88) (background light))
     :foreground "color-196" :weight bold)
    (((class color) (min-colors 88) (background dark))
     :foreground "color-196"  :weight bold)
    (((class color) (min-colors 16) (background light))
     :foreground "red" :weight bold)
    (((class color) (min-colors 16) (background dark))
     :foreground "red" :weight bold)
    (((class color) (min-colors 8))
     :foreground "red" :weight bold)
    (t :inverse-video t))
  "Basic face for highlighting fixme's."
    :group 'fix-me)
  

(font-lock-add-keywords nil '(("\\(TODO\\|todo\\|fixme\\|FIXME\\|BUG\\) " 0
                               'fixme-face t )))


