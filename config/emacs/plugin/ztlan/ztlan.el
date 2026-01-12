;;; ztlan.el --- 底部状态栏插件

;;; Commentary:
;; 一个简单的底部状态栏插件，显示一个红色背景的状态栏，
;; 只有一个按钮文本为'1'，点击后打开左侧窗口

;;; Code:

;; 加载按钮动作
(load (expand-file-name "actions/1/button1.el" (file-name-directory load-file-name)))

(defface ztlan-status-face
  '((t :background "red" :foreground "white"))
  "状态栏的面部设置")

(defvar ztlan--status-buffer nil
  "底部状态栏缓冲区")

(defun ztlan--create-status-buffer ()
  "创建底部状态栏缓冲区"
  (let ((buffer (get-buffer-create "*ztlan-status*")))
    (with-current-buffer buffer
      (setq mode-line-format nil)
      (setq header-line-format nil)
      (setq cursor-type nil)
      (setq window-size-fixed 'height))
    buffer))

(defun ztlan--render-status-bar ()
  "渲染底部状态栏内容"
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap [mouse-1] 'ztlan--button1-action)
    (with-current-buffer ztlan--status-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "1"
                            'face 'ztlan-status-face
                            'mouse-face 'highlight
                            'help-echo "点击打开/关闭侧边栏"
                            'keymap keymap))
        (insert (propertize " " 'display '((space :align-to (- right-fringe 0))) 'face 'ztlan-status-face))))))

(defun ztlan-init ()
  "初始化并显示底部状态栏"
  (interactive)
  ;; 禁用所有窗口的默认 mode-line
  (setq-default mode-line-format nil)
  ;; 创建状态栏缓冲区
  (unless ztlan--status-buffer
    (setq ztlan--status-buffer (ztlan--create-status-buffer)))
  ;; 渲染状态栏内容
  (ztlan--render-status-bar)
  ;; 在底部显示状态栏窗口
  (display-buffer-in-side-window
   ztlan--status-buffer
   '((side . bottom)
     (slot . 0)
     (window-height . 1)
     (window-parameters
      (no-delete-other-windows . t)
      (no-other-window . t)
      (window-preserve-size . t)))))

(provide 'ztlan)

;;; ztlan.el ends here