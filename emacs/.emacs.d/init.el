(setq gc-cons-threshold most-positive-fixnum)
;; reset gc-cons-threshold
(run-with-idle-timer
 10 nil
 (lambda ()
   ;; recommended amount by flx
   (setq gc-cons-threshold (car (get 'gc-cons-threshold 'standard-value)))
   (message "gc-cons-threshold restored to %S"
            gc-cons-threshold)))

(defvar hmg23:file-name-handler-alist-backup file-name-handler-alist)
(setq file-name-handler-alist nil)

(defun hmg23:restore-file-name-handler-alist ()
  (when hmg23:file-name-handler-alist-backup
    (setq file-name-handler-alist (cl-union hmg23:file-name-handler-alist-backup
                                            file-name-handler-alist))
    (setq hmg23:file-name-handler-alist-backup nil)))

(add-hook 'after-init-hook #'hmg23:restore-file-name-handler-alist)
(add-hook 'desktop-save-mode-hook #'hmg23:restore-file-name-handler-alist)



(load "~/.emacs.d/my-functions")
(setq package-check-signature nil)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
;; load up the modes
(when (>= emacs-major-version 24)
  (require 'package)
  (setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                           ("melpa" . "http://melpa.org/packages/"))))

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

(use-package docker-cli
  :config
  (defun fsi-args () '("fsi"))
  (setq docker-cli-commands-alist
        (append docker-cli-commands-alist '((fsi
                                             :command "/usr/bin/dotnet"
                                             :arguments-compose-func fsi-args
                                             )))))


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
  (set-frame-font "Anonymice Nerd Font:size=35:antialias=true:autohint=true")
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

(use-package ansi-color
  :config
  (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
  (advice-add 'shell-command :after #'ansi-color-apply-on-minibuffer-advice)
  (defun colorize-compilation-buffer ()
    (toggle-read-only)
    (ansi-color-apply-on-region compilation-filter-start (point))
    (toggle-read-only))
  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer))

(use-package persistent-scratch
  :config
  (persistent-scratch-setup-default)
  (persistent-scratch-autosave-mode 1))

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(advice-add 'shell-command :after #'ansi-color-apply-on-minibuffer-advice)

(require 'my-seas "~/.emacs.d/my-modes/my-seas")
(require 'mail-and-eww "~/.emacs.d/my-modes/mail-and-eww.el")
(require 'my-scripting "~/.emacs.d/my-modes/my-scripting")


;; These are outside of the use-package system
(setq load-prefer-newer t
      vc-handled-backends nil
      custom-file "~/.emacs.d/custom.el"
      use-dialog-box nil)

(setq recentf-max-saved-items 200
      recentf-max-menu-items 15)
(recentf-mode t)

;; for some global modes modes
(setq uniquify-buffer-name-style 'forward)
(global-prettify-symbols-mode 1)
(global-visual-line-mode 1)
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
                                        ;(nlinum-mode t)
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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (evil-commentary evil-magit magit ahk-mode evil-colemak-basics evil-anzu evil-easymotion key-chord evil-owl rainbow-delimiters auto-compile colemak-evil exec-path-from-shell nord-theme elfeed spaceline posframe hydra evil rjsx-mode jedi elpy page-break-lines use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
