;; -*- lexical-binding: t; -*-;

(use-package ispell
  :straight t
  :after flyspell
  :defer t
  :config
  (setq flyspell-issue-welcome-flag nil)
  (setq-default ispell-program-name "/usr/bin/aspell")
  (setq-default ispell-list-command "list"))

(use-package hydra
  :straight t
  :config
  (defhydra hydra-zoom (global-map "<f2>")
    "zoom"
    ("g" text-scale-increase "in")
    ("l" text-scale-decrease "out")))



(use-package dired
  :commands dired-mode
  :config
  (require 'ech-dired "$HOME/.emacs.d/my-modes/evil-collection-hacks/ech-dired.el")
  (ech-dired-setup)
  (setq find-ls-option '("-print0 | xargs -0 ls -alhd" . "")))

(use-package dired-subtree
  :straight t
  :after dired
  :commands (dired-subtree-insert))

(use-package comint
  :after (general)
  :config
  (require 'ech-comint "$HOME/.emacs.d/my-modes/evil-collection-hacks/ech-comint.el")
  (ech-comint-setup))

(use-package anzu
  :straight t
  :defer t
  :config
  (global-anzu-mode))

(use-package flycheck
  :straight t
  :commands (flycheck-mode)
  :defines (flyspell-issue-welcome-flag)
  :init (global-flycheck-mode))

(use-package eglot
  :straight t
  :hook
  ((js2-mode . #'eglot)
   (js-mode . #'eglot)
   ; (rjsx-mode . #'eglot)
   (go-mode . #'eglot)
   (shell-mode . #'eglot)
   (dockerfile-mode . #'eglot)
   (python-mode . #'eglot)
   (fsharp-mode . #'eglot)
   (ruby-mode . #'eglot)))

(use-package company
  :straight t
  :commands company-mode
  :init
  (global-company-mode)
  :config
  (setq tab-always-indent 'complete)
  (setq company-dabbrev-downcase 0)
  (setq company-idle-delay .2)
  (setq company-tooltip-align-annotations t)
  (setq company-tooltip-limit 20)
  (setq company-minimum-prefix-length 1)
  (setq company-echo-delay 0)
  (setq company-begin-commands '(self-insert-command))
  (setq company-transformers '(company-sort-by-occurrence))

  (with-eval-after-load 'company
    (define-key company-active-map (kbd "M-n") nil)
    (define-key company-active-map (kbd "M-p") nil)
    (define-key company-active-map (kbd "C-n") #'company-select-next)
    (define-key company-active-map (kbd "C-e") #'company-select-previous)))

(use-package company-quickhelp
  :straight t
  :after company
  :commands (company-quickhelp)
  :config
  (company-quickhelp-mode))

(use-package magit
  :straight t
  :commands magit-status
  :config
  (with-eval-after-load 'magit
    (setq magit-file-section-map (make-sparse-keymap))
    ;; (define-key magit-status-mode-map (kbd "SPC") nil)
    (define-key magit-status-mode-map (kbd "C-w") nil)
    ;; (define-key magit-status-mode-map (kbd "u") 'magit-unstage-file)
    )
  (setq magit-completing-read-function 'ivy-completing-read))

(use-package ansi-color
  :straight t
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
  :straight t
  :defer t
  :config (browse-kill-ring-default-keybindings))

(use-package company-lsp
  :straight t
  :requires company
  :after company
  :config
  (push 'company-lsp company-backends)
  (setq company-transformers nil
        company-lsp-async t
        company-lsp-cache-candidates nil))

(use-package auto-compile
  :straight t
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

(use-package uniquify
  :defer t)

(use-package spaceline
  :straight t
  :demand t
  :init
  (setq powerline-default-separator 'wave)
  (setq spaceline-window-numbers-unicode 't)
  :config
  (require 'spaceline-config)
  (spaceline-spacemacs-theme))

(use-package page-break-lines
  :straight t)

(use-package nord-theme
  :straight t
  :config
  ;; (set-frame-font "Anonymice Nerd Font:size=20:antialias=true:autohint=true")
  (add-hook 'after-make-frame-functions
            (lambda (frame) (load-theme 'nord t))))

(load-theme 'nord t)

(use-package ivy
  :straight t
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
  :straight t
  :commands (window-numbering-mode)
  :config
  (window-numbering-mode))

(use-package expand-region
  :straight t
  :commands er/expand-region
  :bind ("C-=" . er/expand-region))

(use-package undo-tree
  :straight t
  :config
  (global-undo-tree-mode 1))

(use-package diff-hl
  :straight t
  :defer t
  :config
  (global-diff-hl-mode))

(use-package autopair
  :straight t
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
  :straight t
  :defer t
  :config
  (editorconfig-mode 1))

(use-package exec-path-from-shell
  :straight t
  :config
  (when (string-equal system-type "gnu/linux")
    (exec-path-from-shell-initialize)))

(straight-use-package 'posframe)
(provide 'my-emacs)
