;; ============================================================================
;; 包管理配置模块
;; ============================================================================

(require 'package)

;; 添加包源
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)

;; 初始化包系统
(package-initialize)

;; 定义需要安装的包列表
(defvar my/packages
  '(evil                ; Vim 模拟
    which-key           ; 快捷键提示
    company             ; 自动补全
    flycheck            ; 语法检查
    lsp-mode            ; Language Server Protocol
    lsp-ui              ; LSP UI 增强
    company-lsp         ; LSP 补全后端
    undo-tree           ; 撤销树
    markdown-mode       ; Markdown 支持
    magit               ; Git 接口
    projectile          ; 项目管理
    counsel             ; 增强的查找
    ivy                 ; 补全框架
    swiper              ; 增强的搜索
    ))

;; 自动安装缺失的包
(defun my/install-packages ()
  "Install all packages in my/packages if they are not already installed."
  (interactive)
  (unless package-archive-contents
    (package-refresh-contents))
  (dolist (package my/packages)
    (unless (package-installed-p package)
      (package-install package))))

;; 启动时自动安装包
(my/install-packages)