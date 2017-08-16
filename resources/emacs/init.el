;; install packages
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)
(defvar my-packages '(clojure-mode cider magit company paredit flycheck flycheck-clojure restclient company-restclient youdao-dictionary))
(dolist (p my-packages)
  (unless (package-installed-p p)
    (package-install p)))


;; enable additional mode and feature
;; autoloaded function, no need to require first
(recentf-mode t)
(desktop-save-mode t)
(global-linum-mode t)
(global-auto-revert-mode t)
(global-company-mode)
(add-to-list 'company-backends 'company-restclient)
(ido-mode t)
(require 'magit)
(setq magit-diff-hide-trailing-cr-characters nil)
;; evaluate cider.el, so that fun & vars available to flycheck-clojure
(require 'cider)
;; flycheck-clojure need your file have no side effect, as it will reload your files automaticlly
(eval-after-load 'flycheck '(flycheck-clojure-setup))
(add-hook 'after-init-hook #'global-flycheck-mode)
;; encoding setting
(prefer-coding-system 'utf-8-unix)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (paredit magit company cider))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
