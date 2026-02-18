;; Load straight.el configuration
(load-file (expand-file-name "straight.el" user-emacs-directory))

;; Load UI configuration
(load-file (expand-file-name "config/ui.el" user-emacs-directory))

;; Load keybindings
(load-file (expand-file-name "config/keybindings.el" user-emacs-directory))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(adwaita))
 '(custom-safe-themes
   '("f1e8339b04aef8f145dd4782d03499d9d716fdc0361319411ac2efc603249326"
	 default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
