;; install packages
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)
(defvar my-packages '(geiser rainbow-delimiters clojure-mode cider magit company expand-region paredit flycheck flycheck-clojure restclient company-restclient youdao-dictionary pyim pyim-wbdict lsp-java company-lsp lsp-ui dap-mode yasnippet ztree))
(dolist (p my-packages)
  (unless (package-installed-p p)
    (package-install p)))


;; enable additional mode and feature
;; autoloaded function, no need to require first
;; build-in lib init
(recentf-mode t)
(desktop-save-mode t)
;; old linum-mode will slow down emacs when large file
(global-display-line-numbers-mode t)
(global-auto-revert-mode t)
(ido-mode t)

;; 3rd part lib init
(require 'expand-region)
(global-set-key (kbd "M-2") 'er/expand-region)

;; java support, lsp-java base on lsp-mode(maintained by same team)
;; below packages manully installed:
;; company-lsp(this more powerfull than company-capf) - support code complete
;; lsp-ui & flycheck - error report
;; lsp-ui - javadoc hover, code action
;; dap-mode - debugger, test runner
;; lsp-java will auto download java language server
(require 'lsp-java)
(setq lsp-inhibit-message t)
(add-hook 'java-mode-hook #'lsp-java-enable)
(add-hook 'lsp-mode-hook 'lsp-ui-mode)
(dap-mode t)
(dap-ui-mode t)
(require 'dap-java)
(require 'lsp-imenu)
(add-hook 'lsp-after-open-hook 'lsp-enable-imenu)
;; (setq lsp-java--workspace-folders (list "/Users/fuchengxu/gitrepo/shimdandy"))

;; yasnippet can be used by company-lsp to support expand snippets on completion
(yas-global-mode t)

;; this need to be called before gesiter init
(setq geiser-active-implementations '(racket))
(global-company-mode)
(add-to-list 'company-backends 'company-restclient)
(push 'company-lsp company-backends)
(require 'magit)
(setq magit-diff-hide-trailing-cr-characters nil)
;; evaluate cider.el, so that fun & vars available to flycheck-clojure
(require 'cider)
;; flycheck-clojure need your file have no side effect, as it will reload your files automaticlly
;; (eval-after-load 'flycheck '(flycheck-clojure-setup))
(add-hook 'after-init-hook #'global-flycheck-mode)

;; paredit enable
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)
(add-hook 'clojure-mode-hook           #'enable-paredit-mode)

;; ditaa
;; ditaa rt is prerequest : sudo apt-get install ditaa
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (ditaa . t)))
(setq org-ditaa-jar-path "/usr/share/ditaa/ditaa.jar")

;; direct-linking avoid function var dereference, in the cost of dynamic runtime
;; -Dclojure.compiler.direct-linking=true
(setq cider-clojure-cli-global-options "-A:java9+:dev")

(cider-register-cljs-repl-type 'browser "(do (require 'cljs.repl.browser) (cider.piggieback/cljs-repl (cljs.repl.browser/repl-env)))" 'cider-check-nashorn-requirements)

;; webi input method
;; to registe pyim in input method list, a default small pinyin dict(pyim-pymap) included
;; Issue: in pyim dicts are not categorilized, ascii seq with different prefix to seprate different input scheme: pinying or wubi...
(require 'pyim)
;; scheme major to set ascii seq prefix & other info (these other info is in an abuse)
(setq pyim-default-scheme 'wubi)
(require 'pyim-wbdict)
;;(pyim-wbdict-gb2312-enable)
(pyim-wbdict-gbk-enable)
;; dict which support offen used words
;; (require 'pyim-basedict)
;; (pyim-basedict-enable)


;; encoding setting
(prefer-coding-system 'utf-8-unix)
(setq make-backup-files nil)

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
