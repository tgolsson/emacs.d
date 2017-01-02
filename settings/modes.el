;; C, C++, Ardunio
(add-to-list  'auto-mode-alist '("\\.h$"   . c++-mode))
(add-to-list  'auto-mode-alist '("\\.hpp$"   . c++-mode))
(add-to-list  'auto-mode-alist '("\\.cpp$"   . c++-mode)) 
(add-to-list  'auto-mode-alist '("\\.c$"   . cc-mode))
(add-to-list 'auto-mode-alist '("\.ino$" . arduino-mode))

;; WEB
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode)) 
(add-to-list 'auto-mode-alist '("/mutt" . mail-mode))


;; OTHER MODES
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.el\\'" . emacs-lisp-mode))

(add-to-list 'auto-mode-alist '("\\.js$" . js-mode))
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\.launch\\'" . xml-mode))
(provide 'modes)
