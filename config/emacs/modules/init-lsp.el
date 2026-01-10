;; ============================================================================
;; LSP (Language Server Protocol) 配置模块
;; ============================================================================

(when (package-installed-p 'lsp-mode)
  (require 'lsp-mode)
  
  ;; 启用 LSP
  (add-hook 'prog-mode-hook #'lsp)
  
  ;; LSP 配置
  (setq lsp-prefer-flymake nil  ; 使用 flycheck 而不是 flymake
        lsp-idle-delay 0.5
        lsp-auto-guess-root t
        lsp-log-io nil
        lsp-completion-provider :capf))

(when (package-installed-p 'lsp-ui)
  (require 'lsp-ui)
  
  ;; lsp-ui 配置
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-use-childframe t
        lsp-ui-doc-position 'top
        lsp-ui-doc-include-signature t
        lsp-ui-sideline-enable nil
        lsp-ui-flycheck-enable t
        lsp-ui-flycheck-list-position 'right
        lsp-ui-imenu-enable t
        lsp-ui-imenu-kind-position 'top))

(when (package-installed-p 'company-lsp)
  (require 'company-lsp)
  
  ;; LSP 补全后端
  (push 'company-lsp company-backends)
  
  ;; Company LSP 配置
  (setq company-lsp-cache-candidates 'auto))