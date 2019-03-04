;; load up the modes
;; (my-load-all-in-directory "~/.emacs.d/my-modes/")

(require 'my-general "~/.emacs.d/my-modes/my-general")
(require 'my-evil "~/.emacs.d/my-modes/my-evil")

(use-package auto-compile
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

(use-package uniquify
  ;; Less important than recentf.
  :defer 2)

(use-package spaceline
  :demand t
  :init
  (setq powerline-default-separator 'wave)
  (setq spaceline-window-numbers-unicode 't)
  :config
  (require 'spaceline-segments)
  (require 'spaceline-config)

  (spaceline-compile
    ;; left side
    '(((persp-name
        workspace-number
        window-number)
       :fallback evil-state
       :face highlight-face
       :priority 100)
      (evil-state
       :priority 91)
      (anzu :priority 95)
      auto-compile
      ((buffer-modified buffer-size buffer-id remote-host)
       :face highlight-face
       :priority 98)
      (major-mode :priority 79)
      (process :when active)
      ((flycheck-error flycheck-warning flycheck-info)
       :when active
       :priority 89)
      (minor-modes :when active
                   :priority 9)
      (mu4e-alert-segment :when active)
      (erc-track :when active)
      (version-control :when active
                       :priority 78)
      nyan-cat)
    ;; right side
    '(which-function
      (python-pyvenv :fallback python-pyenv)
      (purpose :priority 94)
      (battery :when active)
      (selection-info :priority 95)
      input-method
      ((buffer-encoding-abbrev
        point-position
        line-column)
       :separator " | "
       :priority 96)
      (global :when active)
      (buffer-position
       :face highlight-face
       :priority 99)
      (hud :priority 99)))

  (setq-default mode-line-format
                '("%e" (:eval (spaceline-ml-main)))))

(use-package ledger
  :defer t
  :config
  (setq ledger-reconcile-default-commodity "£"))

(use-package spaceline-config
  :ensure spaceline
  :config
  (spaceline-spacemacs-theme))

;; (use-package spaceline-all-the-icons
;;   :after spaceline
;;   :config (spaceline-all-the-icons-theme))

(use-package magit
  :defer t
  :commands magit-status
  :bind ("C-c C-g" . magit-status)
  :config
  (setq magit-completing-read-function 'ivy-completing-read))

(use-package ace-jump-mode
  :defer t
  :bind ("C-x SPC" . ace-jump-mode))
(use-package page-break-lines :ensure t)

(use-package projectile
  :after (evil my-evil)
  :init
  (setq projectile-enable-caching t)
  (setq projectile-completion-system 'ivy)
  (setq projectile-enable-caching t)
  :config
  (projectile-mode +1)
  (counsel-projectile-mode +1))

(use-package xresources-theme
  :config
  (add-hook 'after-make-frame-functions
            (lambda (frame) (load-theme 'xresources t))))

(use-package org
  :defer t
  :bind (
         ("C-c o c" . org-capture)
         ("C-c o a" . org-agenda) )
  :init
  (add-hook 'org-mode-hook (lambda () (flyspell-mode t)))
  :config

  (setq org-default-notes-file "~/gorg/notes.org")
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline "~/gorg/todo.org" "Tasks")
           "* TODO %?\n %T \n %i\n" :empty-lines 1)
          ("s" "Scheduled TODO" entry (file+headline "~/gorg/todo.org"  "Scheduled Tasks")
           "* TODO %?\nSCHEDULED: %^t\n  %U" :empty-lines 1)
          ("n" "Note" entry (file+headline "~/gorg/notes.org" "Notes")
           "* NOTE %U\n  %i\n  %a" :empty-lines 1)
          ("w" "Writing Idea" entry (file+headline "~/gorg/writing-ideas.org" "Ideas")
           "* Idea %i\n %t " :empty-lines 1)
          ("j" "Journal Idea" entry (file+headline "~/gorg/notes.org" "Journal Entry")
           "* Journal \n%U\n" :empty-lines 1)
          ("b" "Submission" entry (file+headline "~/gorg/notes.org" "Sumbit")
           "* Submit to \n%U\n" :empty-lines 1)
          ("p" "Pitch Note" entry (file+headline "~/gorg/writing.org" "Pitches")
           "* Pitch to %i \n%t\n" :empty-lines 1)))
  (setq org-log-done t)
  (setq org-agenda-files '("~/gorg/todo.org"
                           "~/gorg/writing.org"
                           "~/gorg/notes.org")))

(use-package yasnippet
  :diminish yas-minor-mode
  :defer t
  :bind ("C-c C-y" . yas-insert-snippet)
  :config
  (setq-default yas-prompt-functions '(yas-ido-prompt yas-dropdown-prompt)))

(use-package sml-mode
  :defer t
  :config
  (setq sml/no-confirm-load-theme t)
  (sml/setup))

(use-package dired
  :defer t
  :commands dired-mode
  :config
  (setq find-ls-option '("-print0 | xargs -0 ls -alhd" . ""))
  (use-package dired-subtree
    :ensure t
    :commands (dired-subtree-insert)))

(use-package anzu
  :defer t
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
  :hook (#'global-flycheck-mode))

(use-package flymake
  :defer t
  :init
  (setq flymake-run-in-place nil))

(use-package company
  :defer t
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
  :init
  (setq tab-always-indent 'complete)
  :config
  (define-key ac-completing-map (kbd "C-n") 'ac-next)
  (define-key ac-completing-map (kbd "C-e") 'ac-previous)
  (progn (ac-config-default)
         (ac-set-trigger-key "TAB")
         (ac-set-trigger-key "<tab>")
         (ac-flyspell-workaround)
         (add-to-list 'ac-modes 'cider-mode)
         (add-to-list 'ac-modes 'cider-repl-mode)
         (add-hook 'cider-mode-hook 'ac-cider-setup)
         (add-hook 'cider-repl-mode-hook 'ac-cider-setup)
         (add-to-list 'ac-sources 'ac-source-yasnippet)))

(use-package ggtags
  :defer t
  :config
  (ggtags-mode +1)
  (counsel-gtags-mode +1))

(use-package markdown-mode
  :defer t
  :init
  (progn
    (add-hook 'markdown-mode-hook 'pandoc-mode)
    (add-hook 'markdown-mode-hook (lambda () (flyspell-mode t)))))

(use-package pandoc-mode
  :defer t
  :init
  (add-hook 'pandoc-mode-hook 'pandoc-load-default-settings))

(use-package swiper
  :defer t
  :bind ("C-s" . counsel-grep-or-swiper))

(use-package ido
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
  :bind (("C-c C-r" . ivy-resume)
         ("C-x C-f" . counsel-find-file)
         ("C-x b" . ivy-switch-buffer)
         ("C-," . ivy-avy)
         ("M-x" . counsel-M-x)
         ("<f12>" . counsel-M-x)
         (:map ivy-minibuffer-map
               ("C-r" . ivy-reverse-i-search))
         ("M-w" . ivy-kill-ring-save))
  :config
  (setq ivy-re-builders-alist
        '((t . ivy--regex-plus))))


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
  :config
  (window-numbering-mode))

(use-package expand-region
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
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package hydra
  :config
  (defhydra hydra-zoom (global-map "<f2>")
    "zoom"
    ("g" text-scale-increase "in")
    ("l" text-scale-decrease "out")))

(use-package nlinum-relative
  :defer t
  :config
  ;; something else you want
  (global-nlinum-mode 1)
  (nlinum-relative-setup-evil)
  (add-hook 'prog-mode-hook 'nlinum-relative-mode))

;; These are outside of the use-package system
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
(fset 'yes-or-no-p 'y-or-n-p)
(nlinum-mode t)
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

(require 'llvm-mode "~/.emacs.d/my-modes/llvm-mode")
(require 'gud-lldb "~/.emacs.d/my-modes/gud-lldb")
(require 'my-shell "~/.emacs.d/my-modes/my-shell")
(require 'my-seas "~/.emacs.d/my-modes/my-seas")
(require 'my-web "~/.emacs.d/my-modes/my-web")
(require 'my-scripting "~/.emacs.d/my-modes/my-scripting")
(require 'my-hydras "~/.emacs.d/my-modes/my-hydras")
(require 'mail-and-eww "~/.emacs.d/my-modes/mail-and-eww")
