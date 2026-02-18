;;; keybindings.el --- 快捷键配置

;;; Commentary:
;; 设置空格为 leader 键，SPC e 切换 treemacs 文件树

;;; Code:

;; 所有 general 相关配置放在 with-eval-after-load 中确保加载顺序
(with-eval-after-load 'general
  ;; 设置 leader 键为空格
  (general-create-definer my-leader-def
    :states '(normal visual)
    :keymaps 'override
    :prefix "SPC")

  ;; SPC e: 切换 treemacs 文件树
  (my-leader-def
    "e" 'treemacs)

  ;; Ctrl+hjkl 窗口跳转
  (general-define-key
    :states '(normal visual insert emacs)
    "C-h" 'windmove-left
    "C-j" 'windmove-down
    "C-k" 'windmove-up
    "C-l" 'windmove-right)

  ;; Ctrl+o 打开文件，Ctrl+Shift+O 切换文件夹侧边栏工作路径，Ctrl+n 新建文件
  (general-define-key
    :states '(normal visual insert emacs)
    "C-o" 'find-file
    "C-S-o" 'treemacs-select-directory
    "C-n" 'find-file)  ; 新建文件用 find-file 即可，输入新文件名就会创建

  ;; Ctrl+q 关闭当前 buffer 和窗口
  (general-define-key
    :states '(normal visual insert emacs)
    "C-q" 'kill-buffer-and-window))

;; treemacs-mode 下的快捷键 (evil normal state)
(with-eval-after-load 'treemacs
  (define-key treemacs-mode-map (kbd "SPC e") 'treemacs)
  ;; 添加 hjkl 风格的移动（treemacs 有自己的导航，这里用 evil 替代）
  (evil-define-key 'normal treemacs-mode-map
    "h" 'treemacs-up-dir
    "j" 'treemacs-next-line
    "k" 'treemacs-previous-line
    "l" 'treemacs-RET-action
    "q" 'treemacs-quit))

(provide 'keybindings)

;;; keybindings.el ends here