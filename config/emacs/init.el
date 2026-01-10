(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq inhibit-startup-screen t)


(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; 确保已安装 evil 包
(unless (package-installed-p 'evil)
  (package-refresh-contents)
  (package-install 'evil))

(require 'evil)
(evil-mode 1)          ; 全局默认开启 Evil 模式
(global-hl-line-mode 1) ; 高亮当前行
(global-display-line-numbers-mode 1)   ; 显示行号

;; 确保已安装 which-key
(unless (package-installed-p 'which-key)
  (package-refresh-contents)
  (package-install 'which-key))
(require 'which-key)
(which-key-mode 1) ; 显示快捷键提示

;; 确保已安装 company
(unless (package-installed-p 'company)
  (package-refresh-contents)
  (package-install 'company))
(global-company-mode 1) ; 自动补全

;; 确保已安装 flycheck
(unless (package-installed-p 'flycheck)
  (package-refresh-contents)
  (package-install 'flycheck))
(global-flycheck-mode 1) ; 语法检查

(global-eldoc-mode 1)   ; 显示函数参数提示

;; C++ 开发环境配置
(add-hook 'c-mode-common-hook
          (lambda ()
            (setq c-default-style "stroustrup"
                  c-basic-offset 4)))

;; 安装并配置 lsp-mode 用于 C++ 智能补全
(unless (package-installed-p 'lsp-mode)
  (package-refresh-contents)
  (package-install 'lsp-mode))
(unless (package-installed-p 'lsp-ui)
  (package-refresh-contents)
  (package-install 'lsp-ui))

(require 'lsp-mode)
(require 'lsp-ui)

;; C++ 模式下启用 LSP
(add-hook 'c++-mode-hook #'lsp)
(add-hook 'c-mode-hook #'lsp)

;; lsp-ui 配置
(setq lsp-ui-doc-enable t
      lsp-ui-doc-use-childframe t
      lsp-ui-doc-position 'top
      lsp-ui-doc-include-signature t
      lsp-ui-sideline-enable nil
      lsp-ui-flycheck-enable t
      lsp-ui-flycheck-list-position 'right)

;; flycheck C++ 配置
(setq flycheck-gcc-language-standard "c++17"
      flycheck-clang-language-standard "c++17")

;; company 配置增强
(setq company-minimum-prefix-length 1
      company-idle-delay 0.2)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
