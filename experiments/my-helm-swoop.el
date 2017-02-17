
(defun my-helm-swoop ()
  "Hide neotree/minimap when doing helm swoop"
  (interactive)
  ;; if Neotree buffer is visible
  (let ((hasneo (member (get-buffer " *NeoTree*") (mapcar 'window-buffer
														  (window-list))))
		(hasmini (member (get-buffer " *MINIMAP*") (mapcar 'window-buffer
														   (window-list)))))
	(when hasneo (neotree-hide))
	(when hasmini (minimap-kill))
	(helm-swoop)
	(when hasneo (neotree-show)
		  (switch-window))
	(when hasmini (minimap-create))
   ))


(bind-key "C-c M-s" 'my-helm-swoop)
