;;; MY-GO-MODE --- Summary
;;
;; Author: Tom Olsson <tom.olsson@embark-studios.com>
;; Copyright © 2019, Tom Olsson, all rights reserved.
;; Created:  7 November 2019
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(use-package go-mode
  :mode "\\.go\\'"
  :init
  (use-package company-go)
  (use-package flycheck-golangci-lint)
  (use-package go-eldoc)
  (use-package go-gopath)
  (use-package go-impl)
  (use-package go-projectile)
  (use-package godoctor)
  (use-package lsp-mode)
  :config
  (add-hook 'go-mode-hook
            (lambda ()
              ;; (define-key go-mode-map (kbd "C-c C-e") #'go-gopath-set-gopath)
              ;; (add-hook 'before-save-hook 'gofmt-before-save)
              ;; (go-eldoc-setup)
              (company-mode 1)
              ;; (go-projectile-tools-add-path)

              (make-local-variable 'company-backends)
              (set (make-local-variable 'company-backends) '(company-lsp))
              (setq lsp-prefer-flymake nil)
              (lsp)

              (add-hook 'before-save-hook 'gofmt-before-save))))
              (lsp-mode 1)
))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; my-go-mode.el ends here
