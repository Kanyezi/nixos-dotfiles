;; ============================================================================
;; 自动补全配置模块
;; ============================================================================

;; Company 配置
(when (package-installed-p 'company)
  (require 'company)
  
  ;; 全局启用 Company
  (global-company-mode 1)
  
  ;; Company 配置
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.2
        company-show-numbers t
        company-tooltip-limit 20
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case t
        company-selection-wrap-around t)
  
  ;; 补全快捷键
  (with-eval-after-load 'company
    (define-key company-active-map (kbd "\C-n") 'company-select-next)
    (define-key company-active-map (kbd "\C-p") 'company-select-previous)
    (define-key company-active-map (kbd "\C-d") 'company-show-doc-buffer)
    (define-key company-active-map (kbd "<tab>") 'company-complete)))

;; Ivy/Counsel/Swiper 配置
(when (package-installed-p 'ivy)
  (require 'ivy)
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t
        ivy-count-format "%d/%d "
        ivy-height 20))

(when (package-installed-p 'counsel)
  (require 'counsel)
  (counsel-mode 1))

(when (package-installed-p 'swiper)
  (require 'swiper))