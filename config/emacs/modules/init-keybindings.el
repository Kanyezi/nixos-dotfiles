;; ============================================================================
;; 快捷键配置模块
;; ============================================================================

;; 使用 which-key 显示快捷键提示
(when (package-installed-p 'which-key)
  (require 'which-key)
  (which-key-mode 1)
  (setq which-key-idle-delay 0.5))

;; 常用快捷键
(global-set-key (kbd "C-x C-b") 'ibuffer)  ; 更好的缓冲区列表
(global-set-key (kbd "M-/") 'hippie-expand) ; 智能展开

;; 窗口管理
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)

;; 缩放字体
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)