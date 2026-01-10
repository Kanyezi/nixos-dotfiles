;; ============================================================================
;; Evil 模式配置模块
;; ============================================================================

(when (package-installed-p 'evil)
  (require 'evil)
  
  ;; 启用 Evil 模式
  (evil-mode 1)
  
  ;; 设置初始状态为 normal 模式
  (setq evil-default-state 'normal)
  
  ;; 在特定模式下使用 Emacs 状态
  (dolist (mode '(ag-mode
                  flycheck-error-list-mode
                  git-rebase-mode))
    (evil-set-initial-state mode 'emacs))
  
  ;; Evil 模式快捷键增强
  (with-eval-after-load 'evil-maps
    (define-key evil-motion-state-map (kbd "/") 'swiper)
    (define-key evil-normal-state-map (kbd "C-p") 'counsel-find-file)
    (define-key evil-insert-state-map (kbd "C-e") 'end-of-line)
    (define-key evil-insert-state-map (kbd "C-a") 'beginning-of-line)))