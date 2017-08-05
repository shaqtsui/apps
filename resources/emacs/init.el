;; install packages
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)
(defvar my-packages '(clojure-mode cider magit company paredit))
(dolist (p my-packages)
  (unless (package-installed-p p)
    (package-install p)))


;; enable additional mode and feature
(require 'recentf)
(recentf-mode t)
(desktop-save-mode t)
(global-linum-mode t)
(global-company-mode)
(require 'ido)
(ido-mode t)
(setq magit-diff-hide-trailing-cr-characters nil)
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
