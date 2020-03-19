;; prepare use-package
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)


(setq use-package-verbose t)
(setq use-package-always-defer t)


(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq exec-path (append exec-path '("/usr/local/bin")))

(use-package env
  :config
  (setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH"))))

;; install package from source
;; 2 approach to use:
;; :quelpa to replace :ensure  - can config details
;; change ensure function & use :ensure - not recommand, :quelpa ignored, cann't config details
(use-package quelpa-use-package
  :ensure t
  :demand t
  :config (setq quelpa-update-melpa-p nil))

;; jupyter-run-repl will start kernel in active directory not emacs process's directory
;; so run it when u open that project's direcory
;; debug: (setq jupyter--debug t)
;; jupyter-run-repl not in jupyter.el, it's in jupyter-repl.el
(use-package jupyter
  :ensure t)

;; subpkg of jupyter, here only to config it
(use-package jupyter-repl
  :config
  (setq exec-path (append exec-path '("/Users/fuchengxu/.julia/conda/3/bin"))))

(use-package org
  :config
  (setq org-confirm-babel-evaluate nil)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (ditaa . t)
     (jupyter . t))))

;; seems hooked with files in lsp project folder
(use-package lsp-mode
  :ensure t
  :config
  (setq lsp-log-io t))

;; company-lsp(this more powerfull than company-capf) - support code complete
;; autoconfiged by lsp-mode
(use-package company-lsp
  :ensure t)

;; lsp-ui & flycheck - error report
;; lsp-ui - javadoc hover, code action
;; autoconfiged by lsp-mode
(use-package lsp-ui
  :ensure t)

;; dap-mode - debugger, test runner
(use-package dap-mode
  :ensure t)


;; lsp provided by lsp-mode
;; after julia-mode & direct config
;; defer t will cause not register error
;; register lsp-client
(use-package lsp-julia
  :quelpa (lsp-julia :fetcher github :repo "gdkrmr/lsp-julia")
  :demand t
  :after (julia-mode)
  :init
  (setq lsp-julia-package-dir "~/Desktop/Projects/appjulia")
  ;; (setq lsp-julia-package-dir "~/.emacs.d/quelpa/build/lsp-julia/languageserver")
  )


;; from JuliaEditorSupport
(use-package julia-mode
  :ensure t)

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
  :config
  (treemacs-tag-follow-mode t)
  (treemacs-git-mode 'simple))

(use-package geiser
  :ensure t
  :config
  (setq geiser-active-implementations '(racket)))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; auto-mode-alist code alread in autoload
(use-package clojure-mode
  :ensure t)

(use-package hydra
  :ensure t)

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
  :config
  (setq cider-clojure-cli-global-options "-A:java9+:dev")
  (cider-register-cljs-repl-type 'browser "(do (require 'cljs.repl.browser) (cider.piggieback/cljs-repl (cljs.repl.browser/repl-env)))" 'cider-check-nashorn-requirements))

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
  :ensure t)

(use-package company-restclient
  :ensure t
  :after (company restclient)
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
  :ensure t)

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

(use-package desktop
  :hook (after-init . desktop-save-mode))
(desktop-save-mode t)

;; old linum-mode will slow down emacs when large file
(use-package display-line-numbers
  :hook (after-init . global-display-line-numbers-mode))

(use-package autorevert
  :hook (after-init . global-auto-revert-mode))

(use-package ido
  :hook (after-init . ido-mode))

(use-package winner
  :hook (after-init . winner-mode))

(use-package scroll-bar
  :demand t
  :hook (after-init . scroll-bar-mode))
(scroll-bar-mode -1)

;; ditaa
;; ditaa rt is prerequest : sudo apt-get install ditaa
;; mac os: brew install ditaa
;; mac jar path: /usr/local/Cellar/ditaa/0.11.0/libexec/ditaa-0.11.0-standalone.jar
(use-package ob-ditaa
  :config
  (setq org-ditaa-jar-path "/usr/share/ditaa/ditaa.jar"))

(use-package mule-cmds
  :config
  (prefer-coding-system 'utf-8-unix))


(use-package files
  :config
  (setq make-backup-files nil))

(use-package faces
  :config
  (set-face-attribute 'default nil :height 130))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(custom-enabled-themes (quote (wombat)))
 '(emms-setup-default-player-list (quote (emms-player-vlc emms-player-vlc-playlist)) t)
 '(package-selected-packages
   (quote
    (ztree youdao-dictionary use-package rainbow-delimiters pyim-wbdict projectile magit lsp-ui lsp-java geiser flycheck-clojure expand-region dap-mode company-restclient company-lsp clj-refactor cider-hydra ace-window))))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
