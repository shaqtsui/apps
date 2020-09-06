;; prepare use-package
(require 'package)
(setq package-archives '(("gnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))

(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(setq use-package-verbose t)
(setq use-package-always-defer t)

(setq mac-command-modifier 'super)
(setq ns-function-modifier 'hyper)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq exec-path (append exec-path '("/usr/local/bin")))

;; install package from source
;; 2 approach to use:
;; :quelpa to replace :ensure  - can config details
;; change ensure function & use :ensure - not recommand, :quelpa ignored, cann't config details
(use-package quelpa-use-package
  :ensure t
  :demand t
  :config (setq quelpa-update-melpa-p nil))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Decision Tree Like app ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; conflict with edebug key binding
(use-package lispy
  :disabled
  :ensure t
  :hook ((clojure-mode emacs-lisp-mode) . lispy-mode))

(use-package which-key
  :ensure t
  :hook (after-init . which-key-mode))

(use-package ace-link
  :ensure t
  :hook (after-init . ace-link-setup-default))

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

(use-package avy
  :ensure t
  :bind
  (("M-g w" . avy-goto-word-1)
   ("M-g f" . avy-goto-line)))

(use-package hydra
  :ensure t)

(use-package company
  :ensure t
  :hook (after-init . global-company-mode)
  :config
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.0))

(use-package ace-window
  :ensure t
  :bind ("M-o" . ace-window))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; lsp related ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package lsp-mode
  :ensure t
  :config
  (setq lsp-log-io t)
  (setq gc-cons-threshold 100000000)
  (setq read-process-output-max (* 1024 1024))
  (setq lsp-enable-file-watchers nil)
  )

(use-package lsp-ivy
  :ensure t)

(use-package lsp-treemacs
  :ensure t
  :config
  (lsp-treemacs-sync-mode 1))

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
  ;; :quelpa (lsp-julia :fetcher github :repo "gdkrmr/lsp-julia" :files (:defaults "languageserver"))
  :quelpa (lsp-julia :fetcher github :repo "gdkrmr/lsp-julia")
  :demand t
  :after (julia-mode)
  :init
  (setq lsp-julia-package-dir "/Users/fuchengxu/Desktop/Projects/appjulia")
  ;; fix error in : LanguageServer.FoldingRangeCapabilities
  (setq lsp-enable-folding t)
  (setq lsp-folding-range-limit 100)
  :hook (julia-mode . lsp-deferred)
  )

;; lsp-java base on lsp-mode(maintained by same team)
;; lsp-java will auto download java language server
;; lsp is autoload provided by lsp-mode
;; java-mode is not a pkg, dummy here to delay the load
(use-package lsp-java
  :ensure t
  :after (java-mode)
  :hook (java-mode . lsp-deferred)
  :config
  (use-package dap-java
    :after (java-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; julia related ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; from JuliaEditorSupport
(use-package julia-mode
  :ensure t)

(use-package julia-repl
  :ensure t)

;; jupyter-run-repl will start kernel in active directory not emacs process's directory
;; so run it when u open that project's direcory
;; debug: (setq jupyter--debug t)
;; jupyter-run-repl not in jupyter.el, it's in jupyter-repl.el
;; demand to config it before executing org code block
(use-package jupyter
  :ensure t
  :demand t
  :config
  (setq exec-path (append exec-path '("/Users/fuchengxu/.julia/conda/3/bin")))
  ;; org-babel-load-languages configed base on available pkgs of name: ob-*
  (use-package ob-jupyter
    :demand t
    :config
    (setq org-babel-default-header-args:jupyter-julia '(;; (:tangle . "yes")
                                                        (:shebang . "#!/bin/bash")
                                                        (:padline . "yes")
                                                        (:comments . "both")
                                                        (:results . "value")
                                                        (:async . "yes")
                                                        (:session . "*julia*")
                                                        (:kernel . "julia-1.5")))
    (org-babel-jupyter-override-src-block "julia")
    ))

(use-package ein
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; clojure related ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; auto-mode-alist code alread in autoload
(use-package clojure-mode
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

;; flycheck-clojure need your file have no side effect, as it will reload your files automaticlly
;; evaluate cider.el first, so that fun & vars available to flycheck-clojure
(use-package flycheck-clojure
  :ensure t
  :after (flycheck clojure-mode)
  :config
  (flycheck-clojure-setup))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; other pkg ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auto-mode-alist code alread in autoload
(use-package yaml-mode
  :ensure t)

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

(use-package ivy-hydra
  :ensure t
  :after (hydra counsel))

(use-package treemacs
  :ensure t
  :config
  (treemacs-tag-follow-mode t)
  (treemacs-git-mode 'simple))

(use-package treemacs-projectile
  :ensure t
  :after (treemacs projectile))

(use-package geiser
  :ensure t
  :config
  (setq geiser-active-implementations '(racket)))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))


(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
  :config
  (setq magit-diff-hide-trailing-cr-characters nil))


(use-package expand-region
  :ensure t
  :bind ("M-2" . er/expand-region))

;; duplicate with lispy
(use-package paredit
  :ensure t
  :hook ((clojure-mode emacs-lisp-mode eval-expression-minibuffer-setup-hook ielm-mode-hook lisp-mode-hook lisp-interaction-mode-hook scheme-mode-hook) . enable-paredit-mode))


(use-package flycheck
  :ensure t
  :hook (after-init . global-flycheck-mode))

(use-package restclient
  :ensure t)

(use-package company-restclient
  :ensure t
  :after (company restclient)
  :config
  (add-to-list 'company-backends 'company-restclient))

(use-package youdao-dictionary
  :ensure t)

;; Input method
(use-package pyim
  :ensure t
  :demand t
  :config
  ;; add another dict(ping ying, include additional offen used words)
  (use-package pyim-basedict
    :disabled
    :ensure nil
    :demand t
    :config (pyim-basedict-enable))
  ;; add another dict(wu bi)
  (use-package pyim-wbdict
    :ensure t
    :demand t
    :config
    ;;(pyim-wbdict-gb2312-enable)
    (pyim-wbdict-v98-enable))
    ;; popup is slow when show in large file
    (setq pyim-page-tooltip 'minibuffer)
    (setq default-input-method "pyim")
    (setq pyim-default-scheme 'wubi)
  )

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
  (setq projectile-project-search-path '("~/Desktop/Projects/"))
  ;; projectile-auto-discover not work so manually trigger here
  (projectile-discover-projects-in-search-path)
  (setq projectile-completion-system 'ivy))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; built-in packages ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package env
  :config
  (setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
  (setenv "JULIA_NUM_THREADS" "4")
  )

(use-package org
  :config
  (setq org-confirm-babel-evaluate nil)
  (use-package org-tempo))

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

(use-package winner
  :hook (after-init . winner-mode))

(use-package scroll-bar
  ;;:demand t
  :hook (after-init . scroll-bar-mode))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
    (ztree youdao-dictionary use-package rainbow-delimiters pyim-wbdict projectile magit lsp-ui lsp-java geiser flycheck-clojure expand-region dap-mode company-restclient company-lsp clj-refactor cider-hydra ace-window)))
 '(safe-local-variable-values (quote ((flycheck-disabled-checkers emacs-lisp-checkdoc)))))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
