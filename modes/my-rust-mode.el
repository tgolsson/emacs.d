;;; RUST-MODE --- Summary
;;
;; Author: Tom Olsson <mail@tomolsson.se>
;; Copyright © 2019, Tom Olsson, all rights reserved.
;; Created: 26 januari 2019
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


(use-package cargo
  :ensure t
  :defer t)

;; (use-package lsp-mode
;;   :commands lsp
;;   :config (require 'lsp-clients)
;;   (setq
;;    lsp-prefer-flymake nil))

(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (define-key lsp-mode-map (kbd "C-c C-l") lsp-command-map)
  (setq
   lsp-ui-sideline-enable t
   lsp-ui-flycheck-enable t
   lsp-ui-flycheck-list-position 'right
   lsp-ui-flycheck-live-reporting t))

;; (use-package company-lsp :commands company-lsp)
(use-package helm-lsp)


;; (use-package flycheck-rust
;;   :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package rust-mode
  ;; :hook (rust-mode . lsp)
  :ensure t
  :defer t
  :init
  (require 'rust-mode)
  ;; (use-package company-racer)
  (use-package flycheck-rust)
  (use-package racer
    :ensure t
    :defer t
    :config
    (setq company-tooltip-align-annotations t))

    (defun my-rust-mode-hook()
      (set (make-local-variable 'compile-command) "cargo run")

      (if (eq system-type 'windows-nt)
          (progn
            (add-to-list 'exec-path "C:/Users/Tom/.cargo/bin"))
        (progn
          (add-to-list 'exec-path "~/.cargo/bin")))

      (setq lsp-rust-analyzer-server-command "rust-analyzer"
            lsp-rust-server 'rust-analyzer)
      (lsp)
      (lsp-mode 1)
      (lsp-lens-mode 1)
      (lsp-rust-analyzer-inlay-hints-mode)
      (company-mode 1)
      (cargo-minor-mode 1)
      (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
      (flycheck-mode 1)
      (hs-minor-mode 1)
      (flycheck-pos-tip-mode 0)
      (flycheck-inline-mode 0)
      (rust-enable-format-on-save)
      (add-to-list 'company-backends '
                   (company-lsp company-yasnippet))
      (setq lsp-ui-doc-use-childframe nil)
      )
    (add-hook 'rust-mode-hook 'my-rust-mode-hook))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; rust-mode.el ends here
