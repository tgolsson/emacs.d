
;; LaTeX settings
(require 'tex)
(require 'auctex-latexmk)
(require 'company-auctex)
(require 'tex-site)

(auctex-latexmk-setup)
(autoload 'reftex-mode "reftex" "RefTeX Minor Mode" t)
(autoload 'turn-on-reftex "reftex" "RefTeX Minor Mode" nil)
(autoload 'reftex-citation "reftex-cite" "Make citation" nil)
(autoload 'reftex-index-phrase-mode "reftex-index" "Phrase Mode" t)


(defun setup-latex-mode()
  (turn-on-reftex)

  (setq-default TeX-master nil)
  (setq auctex-latexmk-inherit-TeX-PDF-mode t)
  (TeX-global-PDF-mode)
  (setq TeX-PDF-mode-on t)
  ;; Force PDF

  
  (company-mode 1)
  (TeX-global-PDF-mode t)
  (visual-line-mode 1)
  (flyspell-mode 1)
  (LaTeX-math-mode 1)

  
  (company-auctex-init)
  ;; (define-key LaTeX-mode-map (kbd "<backtab>") 'TeX-complete-symbol)
  (add-to-list 'company-backends '(company-auctex-bibs company-auctex-labels
  company-auctex-symbols company-auctex-environments company-auctex-macros company-math-symbols-latex company-yasnippet))

  (setq ispell-program-name "aspell") ; could be ispell as well, depending on your preferences
  (setq ispell-dictionary "english") ; this can obviously be set to any language your spell-checking program supports
  
  
  (setq LaTeX-eqnarray-label "eq"
        LaTeX-equation-label "eq"
        LaTeX-figure-label "fig"
        LaTeX-table-label "tab"
        LaTeX-myChapter-label "chap"
        TeX-auto-save t
        TeX-newline-function 'reindent-then-newline-and-indent
        TeX-parse-self t 
        TeX-auto-save t
        TeX-parse-selflf t
        reftex-plug-into-AUCTeX t
        TeX-pdf-mode
        auctex-latexmk-inherit-TeX-PDF-mode t 
        TeX-style-path
        '("style/" "auto/"
          "/usr/share/emacs21/site-lisp/auctex/style/"
          "/var/lib/auctex/emacs21/"
          "/usr/local/share/emacs/site-lisp/auctex/style/")
        LaTeX-section-hook
        '(LaTeX-section-heading
          LaTeX-section-title
          LaTeX-section-toc
          LaTeX-section-section
          LaTeX-section-label)
        )

  )



 	
(add-hook 'plain-TeX-mode-hook
	  (lambda () (set (make-variable-buffer-local 'TeX-electric-math)
			  (cons "$" "$"))))
(add-hook 'LaTeX-mode-hook
	  (lambda () (set (make-variable-buffer-local 'TeX-electric-math)
			  (cons "\\(" "\\)"))))
(add-hook 'LaTeX-mode-hook 'setup-latex-mode) ; with Emacs latex mode
