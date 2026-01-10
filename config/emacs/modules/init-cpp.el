;; ============================================================================
;; C/C++ 开发配置模块
;; ============================================================================

;; C/C++ 代码风格配置
(add-hook 'c-mode-common-hook
          (lambda ()
            (setq c-default-style "stroustrup"
                  c-basic-offset 4
                  tab-width 4
                  indent-tabs-mode nil)))

;; Flycheck C++ 配置
(when (package-installed-p 'flycheck)
  (require 'flycheck)
  (global-flycheck-mode 1)
  
  (setq flycheck-gcc-language-standard "c++17"
        flycheck-clang-language-standard "c++17"))

;; C++ 模式下启用 LSP
(add-hook 'c++-mode-hook #'lsp)
(add-hook 'c-mode-hook #'lsp)

;; 显示函数参数提示
(global-eldoc-mode 1)

;; C++ 特定快捷键
(add-hook 'c++-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-c") 'compile)
            (local-set-key (kbd "C-c C-r") 'recompile)))