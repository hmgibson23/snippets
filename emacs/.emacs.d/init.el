;; load up the modes
;; (my-load-all-in-directory "~/.emacs.d/my-modes/")

(require 'my-evil "~/.emacs.d/my-modes/my-evil")
(require 'my-general "~/.emacs.d/my-modes/my-general")

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
(require 'spaceline-config)
(spaceline-spacemacs-theme))

(use-package magit
  :defer t
  :commands magit-status
  :config
  (setq magit-completing-read-function 'ivy-completing-read))

(use-package page-break-lines :ensure t)


(use-package xresources-theme
  :config
  (add-hook 'after-make-frame-functions
            (lambda (frame) (load-theme 'xresources t))))

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

(use-package window-numbering
  :config
  (window-numbering-mode))

(use-package expand-region
  :defer t
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
  :ensure t
  :config
  (exec-path-from-shell-initialize))


(require 'my-seas "~/.emacs.d/my-modes/my-seas")
(require 'my-web "~/.emacs.d/my-modes/my-web")
(require 'my-scripting "~/.emacs.d/my-modes/my-scripting")
