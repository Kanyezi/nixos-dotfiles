;; ============================================================================
;; UI 配置模块
;; ============================================================================

;; 禁用工具栏和滚动条
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; 禁用启动画面
(setq inhibit-startup-screen t)

;; 高亮当前行
(global-hl-line-mode 1)

;; 显示行号
(global-display-line-numbers-mode 1)

;; 设置语言环境
(set-language-environment 'Chinese-GB)
(setenv "LANG" "zh_CN.UTF-8")

;; 设置字体
(when (display-graphic-p)
  (set-frame-font "DejaVu Sans Mono 12" nil t))

;; 设置默认编码
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

;; 显示匹配的括号
(show-paren-mode 1)

;; 显示列号
(column-number-mode 1)

;; 更好的滚动体验
(setq scroll-margin 3
      scroll-step 1
      scroll-conservatively 10000
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01)