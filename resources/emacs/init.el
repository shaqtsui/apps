;; install packages
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)
(defvar my-packages '(geiser rainbow-delimiters clojure-mode cider magit company paredit flycheck flycheck-clojure restclient company-restclient youdao-dictionary))
(dolist (p my-packages)
  (unless (package-installed-p p)
    (package-install p)))


;; enable additional mode and feature
;; autoloaded function, no need to require first
;; build-in lib init
(recentf-mode t)
(desktop-save-mode t)
(global-linum-mode t)
(global-auto-revert-mode t)
(ido-mode t)
;; 3rd part lib init
;; this need to be called before gesiter init
(setq geiser-active-implementations '(chicken guile))
(setq cider-cljs-boot-repl "(do (require 'apps.bootstrap.cljs-rt-browser) (apps.bootstrap.cljs-rt-browser/-main))")
(global-company-mode)
(add-to-list 'company-backends 'company-restclient)
(require 'magit)
(setq magit-diff-hide-trailing-cr-characters nil)
;; evaluate cider.el, so that fun & vars available to flycheck-clojure
(require 'cider)
;; flycheck-clojure need your file have no side effect, as it will reload your files automaticlly
(eval-after-load 'flycheck '(flycheck-clojure-setup))
(add-hook 'after-init-hook #'global-flycheck-mode)
;; paredit enable
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)
(add-hook 'clojure-mode-hook           #'enable-paredit-mode)

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
 '(cider-debug-code-overlay-face ((t (:underline (:color foreground-color :style wave))))))
