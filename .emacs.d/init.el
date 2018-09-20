;;(load (expand-file-name "~/quicklisp/slime-helper.el"))
;; Replace "sbcl" with the path to your implementation
;;(setq inferior-lisp-program "/usr/local/bin/sbcl")

;; load up the modes
(my-load-all-in-directory "~/.emacs.d/my-modes/")

(use-package magit
  :ensure t
  :commands magit-status
  :bind ("C-c C-g" . magit-status)
  :config
  (setq magit-completing-read-function 'ivy-completing-read))

(use-package ace-jump-mode
  :ensure t
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

(use-package org
  :ensure t
  :bind ("C-c o a" . org-agenda)
  :init
  (add-hook 'org-mode-hook 'pandoc-mode)
  (add-hook 'org-mode-hook (lambda () (flyspell-mode t)))
  :config
  (setq org-log-done t))

(use-package yasnippet
  :diminish yas-minor-mode
  :ensure t
  :bind ("C-c C-y" . yas-insert-snippet)
  :config
  (setq-default yas-prompt-functions '(yas-ido-prompt yas-dropdown-prompt)))

(use-package sml-mode
  :ensure t
  :config
  (setq sml/no-confirm-load-theme t)
  (sml/setup))

(use-package dired
  :ensure nil
  :commands dired-mode
  :config
  (setq find-ls-option '("-print0 | xargs -0 ls -alhd" . ""))
  (use-package dired-subtree
    :ensure t
    :commands (dired-subtree-insert)))

(use-package anzu
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
  :ensure t
  :hook (#'global-flycheck-mode))

(use-package flymake
  :init
  (setq flymake-run-in-place nil))

(use-package company
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
  ;;(add-hook 'after-init-hook 'global-company-mode)
  :config
  ;; (global-company-mode t)
  (define-key company-active-map (kbd "C-n") 'company-select-next-or-abort)
  (define-key company-active-map (kbd "C-p") 'company-select-previous-or-abort)
  (add-to-list 'company-backends 'company-dabbrev-code)
  (add-to-list 'company-backends 'company-yasnippet)
  (add-to-list 'company-backends 'company-files)
  (add-to-list 'company-backends 'company-anaconda)
  (add-to-list 'company-backends 'company-ghc)
  (add-to-list 'company-backends 'company-web-html))

(use-package auto-complete
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
  :init
  (progn
    (add-hook 'markdown-mode-hook 'pandoc-mode)
    (add-hook 'markdown-mode-hook (lambda () (flyspell-mode t)))))

(use-package pandoc-mode
  :ensure t
  :init
  (add-hook 'pandoc-mode-hook 'pandoc-load-default-settings))

(use-package swiper
  :ensure t
  :bind ("C-s" . counsel-grep-or-swiper))

(use-package ido
  :ensure t
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
  :commands er/expand-region
  :bind ("C-=" . er/expand-region))

(use-package shell-pop
  :bind (("C-x t" . shell-pop))
  :config
  (setq shell-pop-shell-type (quote ("ansi-term" "*ansi-term*" (lambda nil (ansi-term shell-pop-term-shell)))))
  (setq shell-pop-term-shell "/bin/bash")
  (shell-pop--set-shell-type 'shell-pop-shell-type shell-pop-shell-type))


(use-package multi-term
  :defer t
  :bind (("C-c t" . multi-term-next)
         ("C-c T" . multi-term))
  :config
  (use-package term
    :config
    (progn
      (setq term-prompt-regexp ".*:.*>.*? "))))

(use-package multiple-cursors
  :config
  (defhydra mc/hydra (:hint nil)
    "
     ^Up^            ^Down^        ^Miscellaneous^
----------------------------------------------
[_p_]   Previous     [_n_]   Next         [_l_] Edit lines
[_P_]   Skip-Prev    [_N_]   Skip-Next    [_a_] Mark all
[_M-p_] Unmark  [_M-n_] Unmark  [_q_] Quit"
  ("l" mc/edit-lines :exit t)
  ("a" mc/mark-all-like-this :exit t)
  ("n" mc/mark-next-like-this)
  ("N" mc/skip-to-next-like-this)
  ("M-n" mc/unmark-next-like-this)
  ("p" mc/mark-previous-like-this)
  ("P" mc/skip-to-previous-like-this)
  ("M-p" mc/unmark-previous-like-this)
  ("q" nil))
(global-set-key (kbd "C->")  'mc/mark-next-like-this))

(use-package imenu
  :bind ("C-c x i" . imenu)
  :config
  (add-to-list 'imenu-generic-expression
             '("Used Packages"
               "\\(^\\s-*(use-package +\\)\\(\\_<.+\\_>\\)" 2))
  (setq imenu-auto-rescan t))

;; These are outside of the use-package system

(autopair-global-mode +1)
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
