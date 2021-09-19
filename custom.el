(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages)
 '(package-selected-packages
   '(pyvenv dap-python elisp-format company-web-slim company-web-jade company-web-html flycheck-pos-tip-mode dap-gdb-lldb dap-hydra dap-go dap-node dap-mode yaml-mode which-key web-mode web-beautify w3m visual-fill-column use-package transient-dwim toml-mode theme-looper spacemacs-theme solarized-theme smooth-scrolling side-notes rustic rtags rotate ron-mode request-deferred rainbow-mode rainbow-delimiters python-mode protobuf-mode prettier-js powershell php-mode pfuture modern-cpp-font-lock material-theme markdown-toc markdown-preview-eww markdown-mode+ magit-todos magit-filenotify lsp-ui lsp-typescript leaf-tree leaf-convert kubernetes jsonnet-mode js2-refactor js2-highlight-vars js-auto-format-mode ibuffer-projectile hydra htmlize html-script-src hlinum hl-block-mode hide-lines helm-w32-launcher helm-swoop helm-smex helm-projectile helm-lsp helm-ls-git helm-fuzzier helm-flyspell helm-flymake helm-flycheck helm-flx helm-etags-plus helm-describe-modes helm-descbinds helm-ag guru-mode guide-key-tip gruvbox-theme graphviz-dot-mode golint godoctor go-projectile go-impl go-gopath gitignore-mode gif-screencast flymake-rust flymake-gjshint flycheck-rust flycheck-pos-tip flycheck-irony flycheck-inline flycheck-golangci-lint flycheck-clang-tidy expand-region exec-path-from-shell emacsql-sqlite elfeed-org elfeed-goodies el-get editorconfig dumb-jump doom-modeline dockerfile-mode diminish dashboard dash-functional darkroom ctags-update csharp-mode copy-as-format conda company-web company-statistics company-quickhelp company-math company-lua company-jedi company-irony-c-headers company-glsl company-emoji cmake-font-lock clang-format cfrs cargo carbon-now-sh blackout benchmark-init beacon bazel-mode bazel base16-theme anzu ag add-node-modules-path))
 '(prettier-js-args
   '("--config" "/home/tgolsson/go/src/github.com/EmbarkStudios/src/services/hive-controller/frontend/prettier.config.js" "-w"))
 '(prettier-js-command
   "/home/tgolsson/go/src/github.com/EmbarkStudios/src/services/hive-controller/frontend/node_modules/prettier/bin-prettier.js")
 '(safe-local-variable-values
   '((user-mail-address . "me@sbg.dev")
     (eval conda-env-activate "py38")
     (python-shell-interpreter . "/home/tgolsson/anaconda3/envs/py38/bin/python")))
 '(warning-suppress-log-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(magit-diff-added ((t (:background "black" :foreground "green3"))))
 '(magit-diff-removed ((t (:background "black" :foreground "red3")))))
