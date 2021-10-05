(defun to/browse-url-win (url &optional new-window)
  (shell-command (concat "start chrome " url)))

(setq git-shell-path (concat "C:\\Program Files\\Git\\bin")
      git-shell-executable (concat git-shell-path "\\bash.exe"))

(to/pushn exec-path git-shell-path "C:/Users/tom.solberg/.cargo/bin")
;; Disable lockfiles to make flask spaz less
(setq create-lockfiles nil
      browse-url-browser-function 'to/browse-url-win)
(to/set-safe w32-pipe-read-delay 0)
(to/set-safe w32-pipe-buffer-size (* 64 1024))
(setq irony-server-w32-pipe-buffer-size (* 64 1024))
