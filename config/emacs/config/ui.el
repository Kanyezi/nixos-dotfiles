;;禁用工具栏
(tool-bar-mode -1)

;;禁用菜单栏
(menu-bar-mode -1)

;;禁用滚动条
(scroll-bar-mode -1)

;;禁用启动界面
(setq inhibit-startup-screen t)

;;禁用底部横栏
(setq-default mode-line-format nil)

;;禁用左右留白（fringe）
(set-fringe-mode 0)

;; ============ 字体设置 ============
(defun my/set-font ()
  "设置字体，优先使用 JetBrainsMono Nerd Font"
  (let ((font (or (car (x-list-fonts "JetBrainsMono Nerd Font"))
                  (car (x-list-fonts "JetBrains Mono"))
                  (car (x-list-fonts "Fira Code"))
                  (car (x-list-fonts "DejaVu Sans Mono")))))
    (when font
      (set-face-attribute 'default nil :font font :height 120)
      (set-face-attribute 'fixed-pitch nil :font font :height 120))))
(my/set-font)

;; 安装并配置 centaur-tabs（每个 buffer 一个标签）
(straight-use-package 'centaur-tabs)
(centaur-tabs-mode 1)
(setq centaur-tabs-set-icons t           ; 显示图标
      centaur-tabs-set-modified-marker t ; 显示修改标记
      centaur-tabs-gray-out-icons 'buffer ; 灰色未激活图标
      centaur-tabs-close-button "×"      ; 关闭按钮样式
      centaur-tabs-height 28)            ; 标签高度

;; 安装并启用 doom-themes
(straight-use-package 'doom-themes)
(setq doom-themes-enable-bold t
      doom-themes-enable-italic t)
(load-theme 'doom-nord t)  ; Nord 风格主题