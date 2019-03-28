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

(package-initialize)

(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)
(pallet-mode t)

(load "~/.emacs.d/my-functions")
(load "~/.emacs.d/init")

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
