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

(setq load-prefer-newer t
      vc-handled-backends nil
      custom-file "~/.emacs.d/custom.el"
      use-dialog-box nil)
