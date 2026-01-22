;;; button1.el --- Button1 click event handler

;;; Commentary:
;; Button1 click logic: open/close treemacs

;;; Code:

(defun ztlan--treemacs-toggle ()
  "Toggle treemacs sidebar."
  (interactive)
  (require 'treemacs)
  (treemacs))

(defun ztlan--button1-action ()
  "Button1 click event: toggle treemacs file explorer"
  (interactive)
  (message "Button 1 clicked - toggling treemacs")
  (ztlan--treemacs-toggle))

(provide 'button1)

;;; button1.el ends here