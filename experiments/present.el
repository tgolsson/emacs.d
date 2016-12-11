(defvar org-ep-slide-tag "slide" "Tag that marks the beginning of slides")
(defvar org-ep-slide-tag-regexp (concat ":" (regexp-quote
                                             org-ep-slide-tag) ":")
  "Regexp-search tag")
(defvar org-ep-presentation-file "" "The slide in progress")
(defvar org-ep-current-slide 0 "The current position in slide")
(defvar org-ep-number-slides 0 "The total number of slides")
(defvar org-ep-organization "" "The organisation for the slide")
(defvar org-ep-author "" "The name of the presenter")
(defvar org-ep-title "" "The title of the presentation")
(defvar org-ep-email "" "The email to the presenter")

(defun org-ep-align-modeline (left center right)
  "Return a string of `window-width' length with left, center and right aligned strings"
  (let* ((center-width (- (/ (+ (window-width) (length center)) 2.0) (length left) 4))
         (right-width (- (window-width) (+ center-width (length left)) 4)))
    ;; BODY
    (format (format "  %%s%%%ds%%%ds  " center-width right-width) left center right)))


(defun org-ep-get-org-keywords ()
  "parse the buffer and return a cons list of (property . value)
from lines like: #+PROPERTY: value"
  (org-element-map (org-element-parse-buffer 'element) 'keyword
    (lambda (keyword) (cons (org-element-property :key
                                                  keyword)
                            (org-element-property
                             :value keyword)))))

(defun org-ep-get-keyword (KEYWORD)
  "get the value of KEYWORD"
  (cdr (assoc KEYWORD (org-ep-get-org-keywords))))


(defun org-ep-start ()
  "Starts the presentation at the current point"
  (interactive)
  (org-easy-presentation-mode t)
  )

(defun org-ep-exit ()
  "Exits the presentation mode, restoring the org-mode buffer"
  (interactive)
  (widen)
  (org-easy-presentation-mode 0)
  (widen)
  )

(defun org-ep-next ()
  "Moves to the next step of the slide"
  (interactive)
  (setq org-ep-current-slide (1+ org-ep-current-slide))
  (force-mode-line-update)
  (if (eobp)
      (org-ep-next-slide)
    (progn
      (org-show-subtree)
      (org-next-visible-heading 1)
      )
    )
  )

(defun org-ep-next-slide ()
  "Show the next slide."
  (interactive)
  (widen)
  (goto-char (line-end-position))
  (when (re-search-forward org-ep-slide-tag-regexp nil t)
    (progn
      (goto-char (line-beginning-position))
      (org-ep-set-counters)
      (org-narrow-to-subtree)
      (org-show-subtree)
      (save-excursion
        (while (not (eobp))
          (progn
            (org-next-visible-heading 1)
            (hide-subtree))
          ))
      )
  (org-next-visible-heading 1)
    )
  )

(defun org-ep-set-counters()
  (hide-lines-matching "#\+")
  (save-excursion
    (progn
      (widen)
      (beginning-of-buffer)
      (setq org-ep-number-slides (count-matches "[*]+ "))
      (org-babel-remove-result)
      ))
  (save-excursion
    (progn
      (beginning-of-line
       (setq org-ep-current-slide (- org-ep-number-slides (count-matches
                                                           "[*]+ ")))
       ))
    )  
  )
(defun org-ep-previous()
  "Moves to the previous step of the slide"
  (interactive)
  (setq org-ep-current-slide (1- org-ep-current-slide))
  (force-mode-line-update)
  (hide-subtree)
  (org-previous-visible-heading 1)
  (if (bobp)
      (org-ep-previous-slide)
    (progn
    )  
    )
  )

(defun org-ep-previous-slide ()
  "Show the next slide."
  (interactive)
  (find-file org-ep-presentation-file)
  (widen)
  (goto-char (line-beginning-position))
  (if (re-search-backward org-ep-slide-tag-regexp nil t)
    (progn
      (goto-char (line-beginning-position))
      (org-ep-set-counters)
      (org-narrow-to-subtree)
      (outline-show-children)
      (while (not (eobp))
        (progn
          (org-next-visible-heading 1)
          (outline-show-children)))))
  (org-narrow-to-subtree)
  )


(defun org-ep-return ()
  (interactive)
  (find-file org-ep-presentation-file)
  (widen)
  (save-excursion
    (progn
      (org-ep-previous-slide)
      (org-ep-execute-slide-at-point)
      )
    )
  )

(defun org-ep-exec-at-point ()
  "Execute the src-block at point"
  (interactive)
  (hide-lines-show-all)
  (org-babel-execute-buffer)
  (hide-lines-matching "#\+")
  )
 
(define-minor-mode org-easy-presentation-mode
  "Toggle presentation mode for the current buffer. This will start the
presentation for the current mode."
  :init-value nil
  :lighter "P"
  :global nil
  
  :keymap
  `(
    (,(kbd "<f4>") . org-ep-exit)
    (,(kbd "<f5>") . org-ep-previous)
    (,(kbd "<f6>") . org-ep-exec-at-point)
    (,(kbd "<f7>") . org-ep-next)
    )
  (if org-easy-presentation-mode
      (progn
        (setq org-ep-author (org-ep-get-keyword "AUTHOR"))
        (setq org-ep-organization (org-ep-get-keyword "ORGANIZATION"))
        (setq org-ep-email (org-ep-get-keyword "EMAIL"))
        (setq org-ep-name (org-ep-get-keyword "NAME"))
        (setq org-ep-title (org-ep-get-keyword "TITLE"))
        (setq header-line-format
              '((:eval (org-ep-align-modeline
                        ;; Left
                        org-ep-title
                        ;; center
                        ""
                        ;; right
                        (concat
                         (format "%i"(symbol-value 'org-ep-current-slide))
                         "/"
                         (format "%i"(symbol-value 'org-ep-number-slides)))
                        ))))
        (setq mode-line-format
              '((:eval (org-ep-align-modeline
                        ;; Left
                        org-ep-author
                        ;; Center
                        org-ep-organization
                        ;; right
                        org-ep-email
                        ))))

        (setq org-confirm-babel-evaluate nil)        
        (global-set-key (kbd "<f9>") 'org-ep-return)
        (set-face-foreground 'org-level-1 "#FFB080")
        (set-face-foreground 'org-level-2 "#FFC0A0")
        (set-face-foreground 'org-level-3 "#FFD0D0")
        (set-face-attribute 'mode-line nil
                            :foreground "#FA8040"
                            :background nil
                            :height 140
                            :font "Roboto Mono-11"
                            :box '(:line-width 1 :color "gray10")
                            )
        (set-face-attribute 'header-line nil
                            :foreground "#FA8040"
                            :background nil
                            :height 140
                            :font "Roboto Mono-11"
                            :box '(:line-width 5 :color "gray10")
                            )
        
        (set-face-attribute 'default nil :background "gray10")
        (setq org-ep-presentation-file (buffer-file-name))
        (visual-line-mode 0)
        (setq org-pretty-entities 1)
        (blink-cursor-mode 0)
        (org-next-visible-heading 1)
        (org-ep-set-counters)
        (org-narrow-to-subtree)
        (org-hide-subtree)
        (show-children)
        )
    (progn
      )
    )
  )

(global-set-key (kbd "<f6>") 'org-ep-start)
