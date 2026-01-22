;; Package configuration
(require 'package)

;; Add MELPA repository
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)

;; Initialize packages
(package-initialize)

;; Ensure packages are available
(unless package-archive-contents
  (package-refresh-contents))

;; Install treemacs if not installed
(unless (package-installed-p 'treemacs)
  (package-install 'treemacs))

;; Install treemacs-evil if evil is installed
(when (package-installed-p 'evil)
  (unless (package-installed-p 'treemacs-evil)
    (package-install 'treemacs-evil)))

;; Install treemacs-projectile if projectile is installed
(when (package-installed-p 'projectile)
  (unless (package-installed-p 'treemacs-projectile)
    (package-install 'treemacs-projectile)))