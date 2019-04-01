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

(use-package rust-mode
  :ensure t
  :defer t
  :init
  (require 'rust-mode)
  (use-package company-racer)
  (use-package flycheck-rust)
  (use-package racer
    :ensure t
    :defer t
    :config
    (define-key rust-mode-map (kbd "M-\"") #'racer-find-definition)
    (add-hook 'racer-mode-hook #'eldoc-mode)
    (setq company-tooltip-align-annotations t)

    )

  (defun my-rust-mode-hook()
    (message "rust-mode")
    (set (make-local-variable 'compile-command) "cargo run")

    (if (eq system-type 'windows-nt)
        (progn
          (add-to-list 'exec-path "C:/Users/Tom/.cargo/bin")
          (setq racer-cargo-home "C:/Users/Tom/.cargo/bin")
          (setq racer-cmd "C:/Users/Tom/.cargo/bin/racer.exe"))
      (progn
        (add-to-list 'exec-path "~/.cargo/bin")
        (setq racer-cargo-home "~/.cargo/bin")
        (setq racer-cmd "~/.cargo/bin/racer")
        (setq racer-rust-src-path
              "/home/tgolsson/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src")
        (setq company-racer-rust-src "/home/tgolsson/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src"))
      )

    (company-mode 1)
    (cargo-minor-mode 1)
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
    (flycheck-mode 1)
    (hs-minor-mode 1)
    (flycheck-pos-tip-mode 0)
    (flycheck-inline-mode 1)
    (rust-enable-format-on-save)
    (add-to-list 'company-backends '
                 (company-racer company-yasnippet))
)

  (add-hook 'rust-mode-hook 'my-rust-mode-hook)
  (add-hook 'rust-mode-hook #'racer-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; rust-mode.el ends here
