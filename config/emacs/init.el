;; Load straight.el configuration
(load-file (expand-file-name "straight.el" user-emacs-directory))

;; Load UI configuration
(load-file (expand-file-name "config/ui.el" user-emacs-directory))

;; Load plugins
(load-file (expand-file-name "plugin/plugin_init.el" user-emacs-directory))