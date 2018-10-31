;;(load (expand-file-name "~/quicklisp/slime-helper.el"))
;; Replace "sbcl" with the path to your implementation
;;(setq inferior-lisp-program "/usr/local/bin/sbcl")

;; load up the modes
(my-load-all-in-directory "~/.emacs.d/my-modes/")
(require 'llvm-mode)
(require 'gud-lldb)
(require 'go-dlv)

(use-package spaceline :ensure t
  :config
  (setq-default mode-line-format '("%e" (:eval (spaceline-ml-main)))))
(use-package spaceline-config :ensure spaceline
  :config
  (spaceline-emacs-theme))
(use-package magit
  :ensure t
  :defer t
  :commands magit-status
  :bind ("C-c C-g" . magit-status)
  :config
  (setq magit-completing-read-function 'ivy-completing-read))

(use-package ace-jump-mode
  :ensure t
  :defer t
  :bind ("C-x SPC" . ace-jump-mode))
(use-package page-break-lines :ensure t)

(use-package projectile
  :ensure t
  :init
  (setq projectile-enable-caching t)
  (setq projectile-completion-system 'ivy)
  (setq projectile-enable-caching t)
  :config
  (projectile-global-mode +1)
  (counsel-projectile-mode +1))

(use-package xresources-theme
  :config
  (add-hook 'after-make-frame-functions
            (lambda (frame) (load-theme 'xresources t))))

(use-package org
  :ensure t
  :defer t
  :bind ("C-c o a" . org-agenda)
  :init
  (add-hook 'org-mode-hook 'pandoc-mode)
  (add-hook 'org-mode-hook (lambda () (flyspell-mode t)))
  :config
  (setq org-log-done t))

(use-package yasnippet
  :diminish yas-minor-mode
  :defer t
  :ensure t
  :bind ("C-c C-y" . yas-insert-snippet)
  :config
  (setq-default yas-prompt-functions '(yas-ido-prompt yas-dropdown-prompt)))

(use-package sml-mode
  :ensure t
  :defer t
  :config
  (setq sml/no-confirm-load-theme t)
  (sml/setup))

(use-package dired
  :defer t
  :ensure nil
  :commands dired-mode
  :config
  (setq find-ls-option '("-print0 | xargs -0 ls -alhd" . ""))
  (use-package dired-subtree
    :ensure t
    :commands (dired-subtree-insert)))

(use-package anzu
  :defer t
  :ensure t
  :bind (("M-%" . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp))
  :config
  (global-anzu-mode))

;; for some global modes modes
(setq uniquify-buffer-name-style 'forward)
(global-prettify-symbols-mode 1)
(global-visual-line-mode 1)

(setq ac-modes '(lisp-mode clojure-mode slime-mode python-mode
                           repl-mode ruby-mode clojurescript-mode
                           emacs-lisp-mode lisp-mode
                           'enh-ruby-mode'web-mode))

(use-package flycheck
  :defer t
  :ensure t
  :hook (#'global-flycheck-mode))

(use-package flymake
  :defer t
  :init
  (setq flymake-run-in-place nil))

(use-package company
  :defer t
  :ensure t
  :commands company-mode
  :init
  (setq tab-always-indent 'complete)
  (setq company-global-modes '(not term-mode ac-modes))
  (setq company-dabbrev-downcase 0)
  (setq company-idle-delay .1)
  (setq company-begin-commands '(self-insert-command))
  (setq company-tooltip-limit 20)
  (setq company-minimum-prefix-length 1)
  (setq company-echo-delay 0)
  (setq company-begin-commands '(self-insert-command))
  (setq company-transformers '(company-sort-by-occurrence))
  :config
  (define-key company-active-map (kbd "C-n") 'company-select-next-or-abort)
  (define-key company-active-map (kbd "C-e") 'company-select-previous-or-abort)
 (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
  (add-to-list 'company-backends 'company-dabbrev-code)
  (add-to-list 'company-backends 'company-yasnippet)
  (add-to-list 'company-backends 'company-files)
  (add-to-list 'company-backends 'company-anaconda)
  (add-to-list 'company-backends 'company-ghc)
  (add-to-list 'company-backends 'company-web-html))

(use-package auto-complete
  :defer t
  :ensure t
  :init
  (setq tab-always-indent 'complete)
  :config
  (progn (ac-config-default)
         (ac-set-trigger-key "TAB")
         (ac-set-trigger-key "<tab>")
         (ac-flyspell-workaround)
         (add-to-list 'ac-modes 'cider-mode)
         (add-to-list 'ac-modes 'cider-repl-mode)
         (add-hook 'cider-mode-hook 'ac-cider-setup)
         (add-hook 'cider-repl-mode-hook 'ac-cider-setup)
         (add-to-list 'ac-sources 'ac-source-yasnippet)))

(use-package markdown-mode
  :ensure t
  :defer t
  :init
  (progn
    (add-hook 'markdown-mode-hook 'pandoc-mode)
    (add-hook 'markdown-mode-hook (lambda () (flyspell-mode t)))))

(use-package pandoc-mode
  :ensure t
  :defer t
  :init
  (add-hook 'pandoc-mode-hook 'pandoc-load-default-settings))

(use-package swiper
  :ensure t
  :defer t
  :bind ("C-s" . counsel-grep-or-swiper))

(use-package ido
  :ensure t
  :defer t
  :init
  (setq ido-vertical-define-keys 'C-n-and-C-p-only)
  (setq ido-enable-flex-matching t)
  (setq ido-show-dot-for-dired t)
  :config
  (ido-mode t)
  (flx-ido-mode t)
  (ido-vertical-mode +1)
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t))

(use-package ivy
  :ensure t
  :bind (("C-c C-r" . ivy-resume)
         ("C-x C-f" . counsel-find-file)
         ("C-x b" . ivy-switch-buffer)
         ("C-," . ivy-avy)
         (:map ivy-minibuffer-map
               ("C-r" . ivy-reverse-i-search))
         ("M-w" . ivy-kill-ring-save)))


(use-package ibuffer
  :init
  (setq ibuffer-switch-to-saved-filter-groups "default")
  (setq ibuffer-shrink-to-minimum-size t)
  (setq ibuffer-always-show-last-buffer nil)
  (setq ibuffer-sorting-mode 'recency)
  (setq ibuffer-use-header-line t)
  :config
  (setq ibuffer-show-empty-filter-groups nil)
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-projectile-set-filter-groups)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic))))
  (setq ibuffer-saved-filter-groups
        '(("default"
           ("emacs-config" (or (filename . ".emacs.d")
                               (filename . "emacs-config")))
           ("Org" (or (mode . org-mode)
                      (filename . "OrgMode")))
           ("code" (filename . "code"))
           ("haskell" (mode . haskell-mode))
           ("scala" (mode . scala-mode))
           ("html" (mode . web-mode))
           ("racket" (filename . "*\.rkt$/"))
           ("lisp" (filename . "*\.lisp$/"))
           ("javascript" (or (mode . js2-mode)
                             (mode . json-mode)))
           ("css" (or (filename . "*\.less$/")
                      (filename . "*\.css$/")))
           ("Magit" (name . "\*magit"))
           ("Config files" (or (filename . "*\.conf$/")
                               (filename . "*\.properties$/")
                               (filename . "*\.config$/")))
           ("ERC" (mode . erc-mode))
           ("emacs" (or
                     (name . "^\\*scratch\\*$")
                     (name . "^\\*Messages\\*$")))
           ("Help" (or (name . "\*Help\*")
                       (name . "\*Apropos\*")
                       (name . "\*info\*")))))))

(use-package window-numbering
  :ensure t
  :config
  (window-numbering-mode))

(use-package expand-region
  :ensure t
  :defer t
  :commands er/expand-region
  :bind ("C-=" . er/expand-region))


(use-package multi-term
  :defer t
  :bind (("C-c t" . multi-term-next)
         ("C-c T" . multi-term))
  :config
  (use-package term
    :config
    (progn
      (setq term-prompt-regexp ".*:.*>.*? "))))

(use-package imenu
  :bind ("C-c x i" . imenu)
  :defer t
  :config
  (add-to-list 'imenu-generic-expression
               '("Used Packages"
                 "\\(^\\s-*(use-package +\\)\\(\\_<.+\\_>\\)" 2))
  (setq imenu-auto-rescan t))

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode 1))
(use-package diff-hl
  :defer t
  :config
  (global-diff-hl-mode))
(use-package autopair
  :defer t
  :config
  (autopair-global-mode +1))

(setq recentf-max-saved-items 200
      recentf-max-menu-items 15)
(recentf-mode t)

(setq backup-directory-alist `(("." . "~/.saves")))
(setq
 make-backup-files t
 delete-old-versions t
 auto-save-interval 20
 kept-new-versions 10
 kept-old-versions 2
 backup-by-copying t
 version-control t)

(setq backup-directory-alist

      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(setq default-major-mode 'text-mode)
(setq text-mode-hook 'turn-on-auto-fill)

(use-package menu-bar
  :config
  (menu-bar-mode 0))
(use-package tool-bar
  :config
  (tool-bar-mode -1))

(use-package ispell
  :defer t
  :config
  (setq flyspell-issue-welcome-flag nil)
  (setq-default ispell-program-name "/usr/bin/aspell")
  (setq-default ispell-list-command "list"))

(use-package editorconfig
  :defer t
  :config
  (editorconfig-mode 1))
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

;; These are outside of the use-package system
(fset 'yes-or-no-p 'y-or-n-p)
(line-number-mode t)
(size-indication-mode t)
(column-number-mode t)
(setq confirm-nonexistent-file-or-buffer nil)
(display-time-mode)
(setq tab-always-indent 'complete)
(add-to-list 'completion-styles 'initials t)
(setq-default indent-tabs-mode nil
              indicate-buffer-boundaries 'left
              indicate-empty-lines t)
(setq-default tab-width 4)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(show-paren-mode 1)
(global-auto-revert-mode 1)
(electric-indent-mode -1)
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq temporary-file-directory "~/.emacs.d/tmp/")
(setq confirm-nonexistent-file-or-buffer nil)
(setq tramp-default-method "scp")
(setq-default default-directory "~/")
(setq ring-bell-function 'ignore)
(setq compilation-ask-about-save nil)
(add-hook 'prog-mode-hook 'subword-mode)
(persistent-scratch-setup-default)
(persistent-scratch-autosave-mode 1)
(setq dired-listing-switches "-lash")
(setq ad-redefinition-action 'accept)
(setq browse-url-browser-function 'eww-browse-url)
(toggle-scroll-bar -1)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
