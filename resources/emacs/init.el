;; prepare use-package
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)


(setq use-package-verbose t)


(use-package julia-mode
  :ensure t
  :defer t)

(use-package lispy
  :ensure t
  :hook ((clojure-mode emacs-lisp-mode) . lispy-mode))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package ace-link
  :ensure t
  :config
  (ace-link-setup-default))

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

(use-package ivy
  :ensure t
  :defer t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  ;; enable this if you want `swiper' to use it
  ;; (setq search-default-mode #'char-fold-to-regexp)
  (global-set-key "\C-s" 'swiper)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (global-set-key (kbd "<f6>") 'ivy-resume)
;;  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  (global-set-key (kbd "<f1> l") 'counsel-find-library)
  (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
  (global-set-key (kbd "C-c g") 'counsel-git)
  (global-set-key (kbd "C-c j") 'counsel-git-grep)
  (global-set-key (kbd "C-c k") 'counsel-ag)
  (global-set-key (kbd "C-x l") 'counsel-locate)
  (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))

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
  (cider-debug-code-overlay-face ((t (:underline (:color foreground-color :style wave)))))
  )

(use-package cider-hydra
  :ensure t
  :hook (clojure-mode . cider-hydra-mode))

(use-package magit
  :ensure t
  :defer t
  :config
  (setq magit-diff-hide-trailing-cr-characters nil))

(use-package company
  :ensure t
  :config
  (global-company-mode))


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
  :after (flycheck)
  :config
  (flycheck-clojure-setup))

(use-package restclient
  :ensure t
  :defer t)

(use-package company-restclient
  :ensure t
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
  :after (pyim)
  :config
  (pyim-wbdict-v98-enable))

;; lsp-java base on lsp-mode(maintained by same team)
;; lsp-java will auto download java language server
(use-package lsp-java
  :ensure t
  :hook (java-mode . lsp)
  :config
  (setq lsp-inhibit-message t))

;; company-lsp(this more powerfull than company-capf) - support code complete
(use-package company-lsp
  :ensure t
  :config
  (push 'company-lsp company-backends))

;; lsp-ui & flycheck - error report
;; lsp-ui - javadoc hover, code action
(use-package lsp-ui
  :ensure t
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (require 'lsp-ui-imenu)
  (add-hook 'lsp-after-open-hook 'lsp-enable-imenu))

;; dap-mode - debugger, test runner
(use-package dap-mode
  :ensure t
  :defer t
  :after (lsp-mode)
  :config
  (dap-mode t)
  (dap-ui-mode t)
  (require 'dap-java))

;; yasnippet can be used by company-lsp to support expand snippets on completion
(use-package yasnippet
  :ensure t
  :defer t)

(use-package ztree
  :ensure t
  :defer t)

(use-package projectile
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package ace-window
  :ensure t
  :bind ("M-o" . ace-window))


;;; built-in packages
(use-package paren
  :config
  (show-paren-mode t))

(use-package recentf
  :config
  (recentf-mode) t)

(use-package windmove
  :config
  (windmove-default-keybindings))

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
