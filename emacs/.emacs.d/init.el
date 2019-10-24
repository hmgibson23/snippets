;; load up the modes
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   ;; '("melpa" . "http://stable.melpa.org/packages/") ; many packages won't show if using stable
   '("melpa" . "http://melpa.milkbox.net/packages/")
   t))

(package-initialize)

(require 'my-evil "~/.emacs.d/my-modes/my-evil")
(require 'my-general "~/.emacs.d/my-modes/my-general")

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
  (set-frame-font "Hack Nerd Font:size=20:antialias=true:autohint=true")
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
  (exec-path-from-shell-initialize))

(use-package posframe)

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(advice-add 'shell-command :after #'ansi-color-apply-on-minibuffer-advice)
(require 'my-seas "~/.emacs.d/my-modes/my-seas")
(require 'mail-and-eww "~/.emacs.d/my-modes/mail-and-eww.el")
(require 'my-scripting "~/.emacs.d/my-modes/my-scripting")
