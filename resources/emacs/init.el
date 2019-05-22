(condition-case
    nil
    (require 'use-package)
  (file-error
   (require 'package)
   (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
   (package-initialize)
   (package-refresh-contents)
   (package-install 'use-package)
   (require 'use-package)))


(use-package geiser
  :ensure t
  :config
  (setq geiser-active-implementations '(racket)))

(use-package rainbow-delimiters
  :ensure t)

(use-package clojure-mode
  :ensure t)

(use-package hydra
  :ensure t)

;; clj-refactor rely on refactor-nrepl, but refactor-nrepl.ns.slam.hound.search cannot be loaded maybe require:  mranderson048.orchard.v0v3v0.orchard.classpath?
(use-package clj-refactor
  :ensure t
  :config
  (add-hook 'clojure-mode-hook 'clj-refactor-mode)
  (cljr-add-keybindings-with-prefix "C-c C-m"))

;; direct-linking avoid function var dereference, in the cost of dynamic runtime
;; -Dclojure.compiler.direct-linking=true
(use-package cider
  :ensure t
  :config
  (setq cider-clojure-cli-global-options "-A:java9+:dev")
  (cider-register-cljs-repl-type 'browser "(do (require 'cljs.repl.browser) (cider.piggieback/cljs-repl (cljs.repl.browser/repl-env)))" 'cider-check-nashorn-requirements))

(use-package cider-hydra
  :ensure t
  :config
  (add-hook 'clojure-mode-hook 'cider-hydra-mode)
  )

(use-package magit
  :ensure t
  :config
  (setq magit-diff-hide-trailing-cr-characters nil))

(use-package company
  :ensure t
  :config
  (global-company-mode))


(use-package expand-region
  :ensure t
  :config
  (global-set-key (kbd "M-2") 'er/expand-region))

(use-package paredit
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
  (add-hook 'scheme-mode-hook           #'enable-paredit-mode)
  (add-hook 'clojure-mode-hook           #'enable-paredit-mode))

(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))


;; flycheck-clojure need your file have no side effect, as it will reload your files automaticlly
;; evaluate cider.el first, so that fun & vars available to flycheck-clojure
(use-package flycheck-clojure
  :ensure t
  :config
  (flycheck-clojure-setup))

(use-package restclient
  :ensure t)

(use-package company-restclient
  :ensure t
  :config
  (add-to-list 'company-backends 'company-restclient))

(use-package youdao-dictionary
  :ensure t)

;; webi input method
;; to registe pyim in input method list, a default small pinyin dict(pyim-pymap) included
;; Issue: in pyim dicts are not categorilized, ascii seq with different prefix to seprate different input scheme: pinying or wubi...
;; scheme major to set ascii seq prefix & other info (these other info is in an abuse)

;; dict which support offen used words
;; (require 'pyim-basedict)
;; (pyim-basedict-enable)
(use-package pyim
  :ensure t
  :config
  (setq pyim-default-scheme 'wubi))

;;(pyim-wbdict-gb2312-enable)
(use-package pyim-wbdict
  :ensure t
  :config
  (pyim-wbdict-v98-enable))

;; lsp-java base on lsp-mode(maintained by same team)
;; lsp-java will auto download java language server
(use-package lsp-java
  :ensure t
  :config
  (setq lsp-inhibit-message t)
  (add-hook 'java-mode-hook 'lsp))

;; company-lsp(this more powerfull than company-capf) - support code complete
(use-package company-lsp
  :ensure t
  :config
  (push 'company-lsp company-backends))

;; lsp-ui & flycheck - error report
;; lsp-ui - javadoc hover, code action
(use-package lsp-ui
  :ensure t
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  (require 'lsp-ui-imenu)
  (add-hook 'lsp-after-open-hook 'lsp-enable-imenu))

;; dap-mode - debugger, test runner
(use-package dap-mode
  :ensure t
  :config
  (dap-mode t)
  (dap-ui-mode t)
  (require 'dap-java))

;; yasnippet can be used by company-lsp to support expand snippets on completion
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode t))

(use-package ztree
  :ensure t)

(use-package projectile
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package ace-window
  :ensure t
  :config
  (global-set-key (kbd "M-o") 'ace-window))




;; enable additional mode and feature
;; autoloaded function, no need to require first
;; build-in lib init
(recentf-mode t)
(desktop-save-mode t)
;; old linum-mode will slow down emacs when large file
(global-display-line-numbers-mode t)
(global-auto-revert-mode t)
(ido-mode t)
(winner-mode t)


;; ditaa
;; ditaa rt is prerequest : sudo apt-get install ditaa
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (ditaa . t)))
(setq org-ditaa-jar-path "/usr/share/ditaa/ditaa.jar")



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
