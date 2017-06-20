;; install packages
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)
;;(add-to-list 'package-archives
;;             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(defvar my-packages '(clojure-mode cider magit))

(dolist (p my-packages)
  (unless (package-installed-p p)
    (package-install p)))


;; enable additional mode and feature
(require 'recentf)
(recentf-mode t)
(desktop-save-mode t)
(global-linum-mode t)

;;(add-to-list 'load-path "~/.emacs.d/emms/lisp")
;;(require 'emms-setup)
;;(emms-all)
;;(emms-default-players)
;;(setq emms-source-file-default-directory "C:/Users/xfcjs/Downloads/MIT/18.06")

;; encoding setting
(prefer-coding-system 'utf-8-unix)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (cider clojure-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
