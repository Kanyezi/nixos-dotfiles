;; Disable package.el in favor of straight.el
(setq package-enable-at-startup nil)

;; Set eln-cache directory (native compilation cache)
(setq native-comp-eln-load-path
      (list (expand-file-name "eln-cache/" "~/.cache/emacs/")))

;; Set auto-save-list directory
(setq auto-save-list-file-prefix (expand-file-name "auto-save-list/.saves-" "~/.cache/emacs/"))