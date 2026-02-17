;; Set straight.el base directory (packages will be installed here)
(setq straight-base-dir "~/.cache/emacs")

;; Straight.el bootstrap
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install and configure evil
(straight-use-package 'evil)
(evil-mode 1)

;; Configure evil
(with-eval-after-load 'evil
  ;; Set evil state for different modes
  (evil-set-initial-state 'treemacs-mode 'emacs)
  (evil-set-initial-state 'dired-mode 'emacs)
  (evil-set-initial-state 'org-mode 'emacs)

  ;; Use C-u as universal argument in evil
  (setq evil-want-C-u-scroll t)

  ;; Use C-w for window operations
  (setq evil-want-C-w-in-emacs-state t)

  ;; Enable evil in minibuffer
  (setq evil-want-minibuffer t))

;; Install treemacs using straight.el
(straight-use-package 'treemacs)

;; Install treemacs-evil for evil integration
(straight-use-package 'treemacs-evil)

;; Install magit for git management
(straight-use-package 'magit)

;; Set transient history file location (used by magit)
(setq transient-history-file (expand-file-name "transient/history.el" "~/.cache/emacs/"))

;; Configure magit
(with-eval-after-load 'magit
  (setq magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
  (setq magit-commit-show-diff t)
  (setq magit-diff-refine-hunk t)
  (setq magit-status-buffer-switch-function 'switch-to-buffer)
  ;; Configure magit status to show in sidebar
  (add-to-list 'display-buffer-alist
               `(,(regexp-quote "*magit: ")
                 (display-buffer-in-side-window)
                 (side . right)
                 (slot . 0)
                 (window-width . 0.3)
                 (window-parameters
                  (no-delete-other-windows . t)
                  (no-other-window . t)
                  (window-preserve-size . t)))))

;; Configure treemacs
(with-eval-after-load 'treemacs
  (setq treemacs-collapse-dirs                   (if (executable-find "python3") 3 0)
        treemacs-deferred-git-apply-delay        0.5
        treemacs-directory-name-transformer      #'identity
        treemacs-display-in-side-window          t
        treemacs-file-event-delay                2000
        treemacs-file-extension-regex            treemacs-last-period-regex-value
        treemacs-file-follow-delay               0.2
        treemacs-file-name-transformer           #'identity
        treemacs-follow-after-init               t
        treemacs-expand-after-init               t
        treemacs-find-workspace-method           'find-for-file-or-pick-first
        treemacs-git-command-pipe                ""
        treemacs-goto-tag-strategy               'refetch-index
        treemacs-header-scroll-indicators        '(nil . "^^^^^^")
        treemacs-hide-dot-git-directory          t
        treemacs-indentation                     2
        treemacs-indentation-string              " "
        treemacs-is-never-other-window           nil
        treemacs-max-git-entries                 5000
        treemacs-missing-project-action          'ask
        treemacs-move-files-by-mouse-dragging    t
        treemacs-move-forward-on-expand          nil
        treemacs-no-png-images                   nil
        treemacs-no-delete-other-windows         t
        treemacs-project-follow-cleanup          nil
        treemacs-persist-file                    (expand-file-name "treemacs-persist" "~/.cache/emacs/")
        treemacs-position                        'left
        treemacs-read-string-input               'from-child-frame
        treemacs-recenter-distance               0.1
        treemacs-recenter-after-file-follow      nil
        treemacs-recenter-after-tag-follow       nil
        treemacs-recenter-after-project-jump     'always
        treemacs-recenter-after-project-expand   'on-distance
        treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
        treemacs-project-follow-into-home        nil
        treemacs-show-cursor                     nil
        treemacs-show-hidden-files               t
        treemacs-silent-filewatch                nil
        treemacs-silent-refresh                  nil
        treemacs-sorting                         'alphabetic-asc
        treemacs-select-when-already-in-treemacs 'move-back
        treemacs-space-between-root-nodes        t
        treemacs-tag-follow-cleanup              t
        treemacs-tag-follow-delay                1.5
        treemacs-text-scale                      nil
        treemacs-user-mode-line-format           nil
        treemacs-user-header-line-format         nil
        treemacs-wide-toggle-width               70
        treemacs-width                           35
        treemacs-width-increment                 1
        treemacs-width-is-initially-locked       t
        treemacs-workspace-switch-cleanup        nil)
  ;; Enable useful modes
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode 'always)
  ;; Enable git mode if git is available
  (when (executable-find "git")
    (if (executable-find "python3")
        (treemacs-git-mode 'deferred)
      (treemacs-git-mode 'simple))))