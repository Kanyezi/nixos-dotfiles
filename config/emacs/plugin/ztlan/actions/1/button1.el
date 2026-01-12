;;; button1.el --- 按钮1的点击事件处理

;;; Commentary:
;; 按钮1点击后的逻辑：打开/关闭左侧侧边栏

;;; Code:

(defvar ztlan--side-buffer "*ztlan-side*"
  "侧边栏缓冲区名称")

;; 定义专用的侧边栏模式
(define-derived-mode ztlan-side-mode special-mode "ztlan-side"
  "ztlan 侧边栏专用模式。")

(defun ztlan--side-buffer-init ()
  "初始化侧边栏缓冲区内容。"
  (with-current-buffer (get-buffer-create ztlan--side-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (propertize "侧边栏\n" 'face 'bold))
      (insert-button "[ 收起 ]"
                     'action (lambda (_) (ztlan--side-hide))
                     'follow-link t
                     'help-echo "点击关闭侧边栏")
      (insert "\n\n")
      (insert "这是一个真正的 side window\n")
      (insert "可以拖动调整大小\n")
      (insert "不会被 C-x 0 删除\n")
      (goto-char (point-min)))
    (setq-local cursor-type nil)
    (ztlan-side-mode)
    (read-only-mode 1)
    (current-buffer)))

(defun ztlan--side-hide ()
  "仅关闭侧边栏。"
  (interactive)
  (when-let ((win (get-buffer-window ztlan--side-buffer)))
    (delete-window win)))

(defun ztlan--button1-action ()
  "按钮1的点击事件：切换侧边栏窗口显示"
  (interactive)
  (if (get-buffer-window ztlan--side-buffer)
      (ztlan--side-hide)
    ;; 否则创建新的 side window
    (display-buffer-in-side-window
     (ztlan--side-buffer-init)
     '((side . left)
       (slot . 0)
       (window-width . 0.18)
       (window-parameters
        (no-delete-other-windows . t)
        (no-other-window . t)
        (window-preserve-size . t))))))

(provide 'button1)

;;; button1.el ends here