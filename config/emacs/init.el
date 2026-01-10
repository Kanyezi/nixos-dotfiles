;; ============================================================================
;; Emacs 主配置文件
;; ============================================================================

;; 添加模块目录到加载路径
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "custom" user-emacs-directory))

;; 加载各模块配置
(load "init-packages" nil t)    ; 包管理
(load "init-ui" nil t)          ; UI 配置
(load "init-evil" nil t)        ; Evil 模式
(load "init-keybindings" nil t) ; 快捷键
(load "init-completion" nil t)  ; 自动补全
(load "init-lsp" nil t)         ; LSP 支持
(load "init-cpp" nil t)         ; C++ 开发

;; 加载自定义配置
(load "custom/custom" nil t)

;; 自定义变量设置（由 Emacs 自动管理）
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it it, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(evil which-key company flycheck lsp-mode lsp-ui company-lsp undo-tree
            markdown-mode magit projectile counsel ivy swiper)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess up it, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )