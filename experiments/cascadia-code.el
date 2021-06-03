
(defvar composition-ligature-table (make-char-table nil))

;; support ligatures, some toned down to prevent hang
(when (version<= "27.0" emacs-version)
  (let ((alist
         '((33 . ".\\(?:\\(==\\|[!=]\\)[!=]?\\)")
           (35 . ".\\(?:\\(###?\\|_(\\|[(:=?[_{]\\)[#(:=?[_{]?\\)")
           (36 . ".\\(?:\\(>\\)>?\\)")
           (37 . ".\\(?:\\(%\\)%?\\)")
           (38 . ".\\(?:\\(&\\)&?\\)")
           (42 . ".\\(?:\\(\\*\\*\\|[*>]\\)[*>]?\\)")
           ;; ;; (42 . ".\\(?:\\(\\*\\*\\|[*/>]\\).?\\)")
           (43 . ".\\(?:\\([>]\\)>?\\)")
           ;; ;; (43 . ".\\(?:\\(\\+\\+\\|[+>]\\).?\\)")
           (45 . ".\\(?:\\(-[->]\\|<<\\|>>\\|[-<>|~]\\)[-<>|~]?\\)")
           ;; ;; (46 . ".\\(?:\\(\\.[.<]\\|[-.=]\\)[-.<=]?\\)")
           (46 . ".\\(?:\\(\\.<\\|[-=]\\)[-<=]?\\)")
           (47 . ".\\(?:\\(//\\|==\\|[=>]\\)[/=>]?\\)")
           ;; ;; (47 . ".\\(?:\\(//\\|==\\|[*/=>]\\).?\\)")
           (48 . ".\\(?:\\(x[a-fA-F0-9]\\).?\\)")
           (58 . ".\\(?:\\(::\\|[:<=>]\\)[:<=>]?\\)")
           (59 . ".\\(?:\\(;\\);?\\)")
           (60 . ".\\(?:\\(!--\\|\\$>\\|\\*>\\|\\+>\\|-[-<>|]\\|/>\\|<[-<=]\\|=[<>|]\\|==>?\\||>\\||||?\\|~[>~]\\|[$*+/:<=>|~-]\\)[$*+/:<=>|~-]?\\)")
           (61 . ".\\(?:\\(!=\\|/=\\|:=\\|<<\\|=[=>]\\|>>\\|[=>]\\)[=<>]?\\)")
           (62 . ".\\(?:\\(->\\|=>\\|>[-=>]\\|[-:=>]\\)[-:=>]?\\)")
           (63 . ".\\(?:\\([.:=?]\\)[.:=?]?\\)")
           (91 . ".\\(?:\\(|\\)[]|]?\\)")
           ;; (92 . ".\\(?:\\([\\n]\\)[\\]?\\)")
           (94 . ".\\(?:\\(=\\)=?\\)")
           ;; (95 . ".\\(?:\\(|_\\|[_]\\)_?\\)")
           (119 . ".\\(?:\\(ww\\)w?\\)")
           (123 . ".\\(?:\\(|\\)[|}]?\\)")
           (124 . ".\\(?:\\(->\\|=>\\||[-=>]\\||||*>\\|[]=>|}-]\\).?\\)")
           (126 . ".\\(?:\\(~>\\|[-=>@~]\\)[-=>@~]?\\)")
           )))
    (dolist (char-regexp alist)
      (set-char-table-range composition-ligature-table (car char-regexp)
                            `([,(cdr char-regexp) 0 font-shape-gstring]))))
  (set-char-table-parent composition-ligature-table composition-function-table))

(defun cascadia-code--enable ()
  "Enable Cascadia Code ligatures in current buffer."
  (setq-local composition-function-table composition-ligature-table)
  (auto-composition-mode t))

(defun cascadia-code--disable ()
  "Disable Cascadia Code ligatures in current buffer."
  (auto-composition-mode nil))

(define-minor-mode cascadia-code-mode
  "Cascadia Code ligatures minor mode"
  :lighter " æ"
  (if cascadia-code-mode
      (cascadia-code--enable)
    (cascadia-code--disable)))


(provide 'cascadia-code)

