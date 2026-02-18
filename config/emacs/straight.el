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

;; Install general.el for keybindings (must be after evil)
(straight-use-package 'general)

;; Configure evil
(with-eval-after-load 'evil
  ;; Set evil state for different modes
  (evil-set-initial-state 'treemacs-mode 'normal)
  (evil-set-initial-state 'dired-mode 'emacs)
  (evil-set-initial-state 'org-mode 'emacs)

  ;; Use C-u as universal argument in evil
  (setq evil-want-C-u-scroll t)

  ;; Use C-w for window operations
  (setq evil-want-C-w-in-emacs-state t)

  ;; Enable evil in minibuffer
  (setq evil-want-minibuffer t)

  ;; 修复自动缩进问题：确保 insert 模式下自动缩进正常工作
  (setq evil-auto-indent t)
  ;; 让 evil 使用 Emacs 的 electric-indent
  (setq evil-electric-indent t))

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

;; ============ LSP 配置 (C++ 代码提示) ============
(straight-use-package 'lsp-mode)
(straight-use-package 'lsp-ui)
(straight-use-package 'company)

;; company 自动补全配置
(with-eval-after-load 'company
  (setq company-idle-delay 0.1           ; 更快弹出
        company-minimum-prefix-length 1  ; 输入1个字符就开始
        company-show-quick-access t))

;; clangd 参数配置
(setq lsp-clients-clangd-args '("--background-index"
                                 "--clang-tidy"
                                 "--header-insertion=iwyu"
                                 "--completion-style=detailed"
                                 "--function-arg-placeholders"
                                 "--fallback-style=llvm"))

;; lsp-ui 配置
(setq lsp-ui-doc-enable t
      lsp-ui-doc-position 'at-point
      lsp-ui-doc-delay 0.5
      lsp-ui-sideline-enable t
      lsp-ui-sideline-show-diagnostics t
      lsp-headerline-breadcrumb-enable nil
      lsp-enable-on-type-formatting nil   ; 禁用 LSP 自动格式化
      lsp-enable-indentation nil)         ; 禁用 LSP 缩进

;; C/C++ 模式自动启动 LSP
(add-hook 'c++-mode-hook 'lsp)
(add-hook 'c-mode-hook 'lsp)

;; 启用 company 全局补全
(add-hook 'after-init-hook 'global-company-mode)

;; ============ Tree-sitter 配置 (语法高亮) ============
;; Emacs 29+ 内置 tree-sitter，但需要检查是否编译支持
(when (and (>= emacs-major-version 29) (fboundp 'treesit-ready-p))
  (setq treesit-language-source-alist
        '((cpp "https://github.com/tree-sitter/tree-sitter-cpp" "v0.22.0")
          (c "https://github.com/tree-sitter/tree-sitter-c" "v0.21.0")))
  
  ;; 自动安装语法解析器
  (dolist (lang '(c cpp))
    (unless (treesit-ready-p lang t)
      (treesit-install-language-grammar lang)))
  
  ;; C++ 模式使用 tree-sitter 高亮
  (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
  (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode)))

;; ============ 自动括号补全 ============
(electric-pair-mode 1)
(setq electric-pair-pairs
      '((?\" . ?\")    ; 双引号
        (?\' . ?\')    ; 单引号
        (?\( . ?\))    ; 小括号
        (?\[ . ?\])    ; 中括号
        (?\{ . ?\})))  ; 大括号

;; ============ 自动缩进 ============
(electric-indent-mode 1)
;; 确保 C/C++ 模式下正确缩进
(setq c-electric-flag t)
(add-hook 'c++-mode-hook (lambda () (c-toggle-electric-state 1)))
(add-hook 'c-mode-hook (lambda () (c-toggle-electric-state 1)))

;; ============ Tab 缩进设置 ============
(setq-default tab-width 4           ; Tab 显示宽度为 4
              indent-tabs-mode t)   ; 使用 Tab 字符缩进
(setq c-basic-offset 4)             ; C/C++ 缩进宽度