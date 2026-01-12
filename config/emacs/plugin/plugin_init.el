;; 插件初始化文件

;; 加载 ztlan 状态栏插件
(load-file (expand-file-name "plugin/ztlan/ztlan.el" user-emacs-directory))
(ztlan-init)