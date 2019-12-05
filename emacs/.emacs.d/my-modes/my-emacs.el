;; -*- lexical-binding: t; -*-;

(use-package ispell
  :after flyspell
  :defer t
  :config
  (setq flyspell-issue-welcome-flag nil)
  (setq-default ispell-program-name "/usr/bin/aspell")
  (setq-default ispell-list-command "list"))

(use-package hydra
  :config
  (defhydra hydra-zoom (global-map "<f2>")
    "zoom"
    ("g" text-scale-increase "in")
    ("l" text-scale-decrease "out")))

(use-package dired
  :defer t
  :commands dired-mode
  :config
  (require 'ech-dired "$HOME/.emacs.d/my-modes/evil-collection-hacks/ech-dired.el")
  (ech-dired-setup)
  (setq find-ls-option '("-print0 | xargs -0 ls -alhd" . "")))

(use-package dired-subtree
  :after dired
  :commands (dired-subtree-insert))

(use-package comint
  :config
  (require 'ech-comint "$HOME/.emacs.d/my-modes/evil-collection-hacks/ech-comint.el")
  (ech-comint-setup))

(use-package anzu
  :defer t
  :config
  (global-anzu-mode))

(use-package flycheck
  :commands (flycheck-mode)
  :defines (flyspell-issue-welcome-flag)
  :hook (#'global-flycheck-mode))

(use-package lsp-mode
  ;; ..

  :config
  (setq lsp-prefer-flymake nil) ;; Prefer using lsp-ui (flycheck) over flymake.

  ;; ..
  )

(use-package lsp-ui
  :requires lsp-mode flycheck
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-use-childframe t
        lsp-ui-doc-position 'top
        lsp-ui-doc-include-signature t
        lsp-ui-sideline-enable nil
        lsp-ui-flycheck-enable t
        lsp-ui-flycheck-list-position 'right
        lsp-ui-flycheck-live-reporting t
        lsp-ui-peek-enable t
        lsp-ui-peek-list-width 60
        lsp-ui-peek-peek-height 25)

  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package lsp-mode
  :commands lsp
  :config

  (add-hook 'go-mode-hook #'lsp)
  (add-hook 'python-mode-hook #'lsp)
  (add-hook 'js2-mode-hook #'lsp)
  (add-hook 'js2-mode-hook #'lsp)
  (message "hooks added")
  (add-hook 'ruby-mode-hook #'lsp))

(use-package company-lsp
  :commands company-lsp
  :config
  (push 'company-lsp company-backends))

(use-package company
  :commands company-mode
  :after (general)
  :init
  (setq tab-always-indent 'complete)
  (setq company-dabbrev-downcase 0)
  (setq company-idle-delay .1)
  (setq company-begin-commands '(self-insert-command))
  (setq company-tooltip-limit 20)
  (setq company-minimum-prefix-length 1)
  (setq company-echo-delay 0)
  (setq company-begin-commands '(self-insert-command))
  (setq company-transformers '(company-sort-by-occurrence))

  :config
  (global-company-mode 1)

  (with-eval-after-load 'company
    (define-key company-active-map (kbd "M-n") nil)
    (define-key company-active-map (kbd "M-p") nil)
    (define-key company-active-map (kbd "C-n") #'company-select-next)
    (define-key company-active-map (kbd "C-e") #'company-select-previous))

  (add-to-list 'company-backends 'company-dabbrev-code)
  (add-to-list 'company-backends 'company-yasnippet)
  (add-to-list 'company-backends 'company-files)
  (add-to-list 'company-backends 'company-elisp)
  (add-to-list 'company-backends 'company-anaconda)
  (add-to-list 'company-backends 'company-ghc)
  (add-to-list 'company-backends 'company-web-html))

(use-package company-quickhelp
  :after company
  :commands (company-quickhelp)
  :config
  (company-quickhelp-mode))

(use-package ggtags
  :commands (ggtags-global-mode)
  :config
  (counsel-gtags-mode +1))

(use-package magit
  :commands magit-status
  :config
  (with-eval-after-load 'magit
    (setq magit-file-section-map (make-sparse-keymap))
    (define-key magit-status-mode-map (kbd "SPC") nil)
    (define-key magit-status-mode-map (kbd "C-w") nil)
    (define-key magit-status-mode-map (kbd "u") 'magit-unstage-file)
    )
  (setq magit-completing-read-function 'ivy-completing-read))

(use-package ansi-color
  :config
  (defun ansi-color-apply-on-buffer ()
    (ansi-color-apply-on-region (point-min) (point-max)))

  (defun ansi-color-apply-on-minibuffer ()
    (let ((bufs (remove-if-not
                 (lambda (x) (string-starts-with (buffer-name x) " *Echo Area"))
                 (buffer-list))))
      (dolist (buf bufs)
        (with-current-buffer buf
          (ansi-color-apply-on-buffer)))))

  (defun ansi-color-apply-on-minibuffer-advice (proc &rest rest)
    (ansi-color-apply-on-minibuffer))

  (advice-add 'shell-command :after #'ansi-color-apply-on-minibuffer-advice))

(use-package browse-kill-ring
  :defer t
  :config (browse-kill-ring-default-keybindings))

(use-package company
  :config
  (setq company-idle-delay 0.3)

  (global-company-mode 1)

  (global-set-key (kbd "C-<tab>") 'company-complete))

(use-package company-lsp
  :requires company
  :config
  (push 'company-lsp company-backends)

   ;; Disable client-side cache because the LSP server does a better job.
  (setq company-transformers nil
        company-lsp-async t
        company-lsp-cache-candidates nil))

(use-package auto-compile
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

(use-package uniquify
  ;; Less important than recentf.
  :defer t)

(use-package spaceline
  :demand t
  :init
  (setq powerline-default-separator 'wave)
  (setq spaceline-window-numbers-unicode 't)
  :config
  (require 'spaceline-config)
  (spaceline-spacemacs-theme))

(use-package page-break-lines :ensure t)
(use-package nord-theme
  :config
  (set-frame-font "Anonymice Nerd Font:size=20:antialias=true:autohint=true")
  (add-hook 'after-make-frame-functions
            (lambda (frame) (load-theme 'nord t))))

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
  :init
  (setq ivy-use-virtual-buffers t)
  (setq ivy-re-builders-alist
        '((t . ivy--regex-plus)))
  (ivy-mode 1)
  :config
  (define-key ivy-minibuffer-map (kbd "C-n") 'ivy-next-line)
  (define-key ivy-minibuffer-map (kbd "C-e") 'ivy-previous-line)
  (define-key ivy-minibuffer-map (kbd "C-r") 'ivy-reverse-i-search)
  (define-key ivy-minibuffer-map (kbd "[backspace]") 'ivy-backward-delete-char))

(use-package window-numbering
  :commands (window-numbering-mode)
  :config
  (window-numbering-mode))

(use-package expand-region
  :commands er/expand-region
  :bind ("C-=" . er/expand-region))

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

(use-package editorconfig
  :defer t
  :config
  (editorconfig-mode 1))

(use-package exec-path-from-shell
  :config
  (when (string-equal system-type "gnu/linux")
    (exec-path-from-shell-initialize)))

(use-package posframe)
(provide 'my-emacs)
