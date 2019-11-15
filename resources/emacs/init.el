;; prepare use-package
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)


(setq use-package-verbose t)


;; use-package steps:
;; 1, download if :ensure t
;; 2, if :after, hook following with :after & END
;; 3, execute :init & hook :config with require status of pkg
;; 4, if :defer = nil, require package
;; Two kinds of pkg: 1, provide set of functions. 2, executed to enhance/change something
;; Note; require pkg should not defaultly required, normally 1st kind of pkg exposed via :command/autoload
;; 2rd kind, required & executed with :after or if autoload function to be executed use one time hook like after-init


(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

;; install package from source
(use-package quelpa-use-package
  :ensure t
  :config (setq quelpa-update-melpa-p nil))

;; seems hooked with files in lsp project folder
(use-package lsp-mode
  :ensure t
  :defer t
  :config
  (setq lsp-log-io t))

;; company-lsp(this more powerfull than company-capf) - support code complete
;; autoconfiged by lsp-mode
(use-package company-lsp
  :ensure t
  :defer t)

;; lsp-ui & flycheck - error report
;; lsp-ui - javadoc hover, code action
;; autoconfiged by lsp-mode
(use-package lsp-ui
  :ensure t
  :defer t)

;; dap-mode - debugger, test runner
(use-package dap-mode
  :ensure t
  :defer t)


;; change ensure function to use quelpa instead of package for installation
(setq use-package-ensure-function 'quelpa)

;; lsp provided by lsp-mode
;; after julia-mode & direct config
(use-package lsp-julia
  :quelpa (lsp-julia :fetcher github :repo "xfcjscn/lsp-julia")
  :ensure t
  :after (julia-mode)
  :init (setq lsp-julia-package-dir "/Users/fuchengxu/Desktop/lsp-julia/languageserver")
  :config
  (add-hook 'julia-mode-hook 'lsp))

(setq use-package-ensure-function 'use-package-ensure-elpa)


(use-package julia-mode
  :ensure t
  :defer t)

(use-package julia-repl
  :ensure t
  :hook (julia-mode . julia-repl-mode))

(use-package flycheck-julia
  :ensure t
  :after (flycheck julia-mode)
  :config (flycheck-julia-setup))


(use-package lispy
  :ensure t
  :hook ((clojure-mode emacs-lisp-mode) . lispy-mode))

(use-package which-key
  :ensure t
  :hook (after-init . which-key-mode))

(use-package ace-link
  :ensure t
  :hook (after-init . ace-link-setup-default))

(use-package emms
  :ensure t
  :defer t
  :custom
  (emms-setup-default-player-list
  '(emms-player-vlc
    emms-player-vlc-playlist)
  "*Default list of players for emms-setup, only vlc.")
  :config
  (emms-all)
  (emms-default-players))

;; ivy, swiper is counsel dependency, auto downloaded
(use-package counsel
  :ensure t
  :bind
  (("C-s" . swiper)
   ("C-c C-r" . ivy-resume)
   ("M-x" . counsel-M-x)
   ("C-x C-f" . counsel-find-file))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  ;; enable this if you want `swiper' to use it
  ;; (setq search-default-mode #'char-fold-to-regexp)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))

(use-package ivy-hydra
  :ensure t
  :after (hydra counsel))

(use-package avy
  :ensure t
  :bind
  (("M-g w" . avy-goto-word-1)
   ("M-g f" . avy-goto-line)))

(use-package treemacs
  :ensure t
  :defer t
  :config
  (treemacs-tag-follow-mode t)
  (treemacs-git-mode 'simple))

(use-package geiser
  :ensure t
  :defer t
  :config
  (setq geiser-active-implementations '(racket)))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; auto-mode-alist code alread in autoload
(use-package clojure-mode
  :ensure t
  :defer t)

(use-package hydra
  :ensure t
  :defer t)

;; clj-refactor rely on refactor-nrepl, but refactor-nrepl.ns.slam.hound.search cannot be loaded maybe require:  mranderson048.orchard.v0v3v0.orchard.classpath?
(use-package clj-refactor
  :ensure t
  :hook (clojure-mode . clj-refactor-mode)
  :config
  (cljr-add-keybindings-with-prefix "C-c C-m"))

;; direct-linking avoid function var dereference, in the cost of dynamic runtime
;; -Dclojure.compiler.direct-linking=true
(use-package cider
  :ensure t
  :defer t
  :config
  (setq cider-clojure-cli-global-options "-A:java9+:dev")
  (cider-register-cljs-repl-type 'browser "(do (require 'cljs.repl.browser) (cider.piggieback/cljs-repl (cljs.repl.browser/repl-env)))" 'cider-check-nashorn-requirements)
  :custom-face
  (cider-debug-code-overlay-face ((t (:underline (:color foreground-color :style wave))))))

(use-package cider-hydra
  :ensure t
  :hook (cider-mode . cider-hydra-mode))

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
  :config
  (setq magit-diff-hide-trailing-cr-characters nil))

(use-package company
  :ensure t
  :hook (after-init . global-company-mode))

(use-package expand-region
  :ensure t
  :bind ("M-2" . er/expand-region))

;; use lispy instead
(use-package paredit
  :disabled
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
  :hook (after-init . global-flycheck-mode))


;; flycheck-clojure need your file have no side effect, as it will reload your files automaticlly
;; evaluate cider.el first, so that fun & vars available to flycheck-clojure
(use-package flycheck-clojure
  :ensure t
  :after (flycheck clojure-mode)
  :config
  (flycheck-clojure-setup))

(use-package restclient
  :ensure t
  :defer t)

(use-package company-restclient
  :ensure t
  :defer t
  :after (company restclient)
  :config
  (add-to-list 'company-backends 'company-restclient))

(use-package youdao-dictionary
  :ensure t
  :defer t)

;; webi input method
;; to registe pyim in input method list, a default small pinyin dict(pyim-pymap) included
;; Issue: in pyim dicts are not categorilized, ascii seq with different prefix to seprate different input scheme: pinying or wubi...
;; scheme major to set ascii seq prefix & other info (these other info is in an abuse)

;; dict which support offen used words
;; (require 'pyim-basedict)
;; (pyim-basedict-enable)
(use-package pyim
  :ensure t
  :defer t
  :config
  (setq pyim-default-scheme 'wubi))

;;(pyim-wbdict-gb2312-enable)
(use-package pyim-wbdict
  :ensure t
  :defer t
  :config
  (pyim-wbdict-v98-enable))


;; lsp-java base on lsp-mode(maintained by same team)
;; lsp-java will auto download java language server
;; lsp is autoload provided by lsp-mode
;; java-mode is not a pkg, dummy here to delay the load
(use-package lsp-java
  :ensure t
  :after (java-mode)
  :config
  (add-hook 'java-mode-hook 'lsp)
  (setq lsp-inhibit-message t))

;; dap-java inside lsp-java, so no :ensure here
;; java-mode is not a pkg, dummy here to delay the load
(use-package dap-java
  :after (java-mode))

;; yasnippet can be used by company-lsp to support expand snippets on completion
;; can't defer, a bug in lsp-mode lsp-enable-snippet didn't require it explicitly
(use-package yasnippet
  :ensure t)

(use-package ztree
  :ensure t
  :defer t)

;; seems projectile is hooked with all files in project folder
(use-package projectile
  :ensure t
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (setq projectile-completion-system 'ivy))

(use-package ace-window
  :ensure t
  :bind ("M-o" . ace-window))


;;; built-in packages
(use-package paren
  :hook (after-init . show-paren-mode))

(use-package recentf
  :hook (after-init . recentf-mode))

(use-package windmove
  :hook (after-init . windmove-default-keybindings))

(desktop-save-mode t)
;; old linum-mode will slow down emacs when large file
(global-display-line-numbers-mode t)
(global-auto-revert-mode t)
(ido-mode nil)
(winner-mode t)


;; ditaa
;; ditaa rt is prerequest : sudo apt-get install ditaa
;; mac os: brew install ditaa
;; mac jar path: /usr/local/Cellar/ditaa/0.11.0/libexec/ditaa-0.11.0-standalone.jar
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
 '(package-selected-packages
   (quote
    (ztree youdao-dictionary use-package rainbow-delimiters pyim-wbdict projectile magit lsp-ui lsp-java geiser flycheck-clojure expand-region dap-mode company-restclient company-lsp clj-refactor cider-hydra ace-window))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cider-debug-code-overlay-face ((t (:underline (:color foreground-color :style wave))))))
