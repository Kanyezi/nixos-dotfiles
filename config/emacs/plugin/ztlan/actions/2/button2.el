;;; button2.el --- Button2 click event handler

;;; Commentary:
;; Button2 click logic: open magit git interface

;;; Code:

(defun ztlan--find-git-repo (dir)
  "Find git repository starting from DIR."
  (let ((git-dir (locate-dominating-file dir ".git")))
    (if git-dir
        git-dir
      (let ((parent (file-name-directory (directory-file-name dir))))
        (and parent (not (string= parent dir))
             (ztlan--find-git-repo parent))))))

(defun ztlan--button2-action ()
  "Button2 click event: open magit status in current directory"
  (interactive)
  (message "Button 2 clicked - opening magit status")
  (require 'magit)
  ;; Try to find git repository
  (let ((git-repo (ztlan--find-git-repo default-directory)))
    (if git-repo
        (magit-status git-repo)
      (message "Not inside Git repository. Please navigate to a git repository first."))))

(provide 'button2)

;;; button2.el ends here