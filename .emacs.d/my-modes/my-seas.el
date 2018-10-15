(use-package cc-mode
  :init
  (setq c-basic-offset 2)
  (setq c-default-style "linux")
  (setq gdb-show-main t)
  :config
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c++-mode-hook 'smartparens-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'flycheck-mode))

(use-package company-irony-c-headers
  :config
  (eval-after-load 'company
    '(add-to-list
      'company-backends '(company-irony-c-headers company-irony))))

(use-package irony
  :defer 1
  :config

  (add-hook 'irony-mode-hook 'irony-eldoc)
  (add-hook 'irony-mode-hook 'company-mode)
  (eval-after-load 'flycheck
    '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

  (defun my-irony-mode-hook ()
    (define-key irony-mode-map [remap completion-at-point]
      'irony-completion-at-point-async)
    (define-key irony-mode-map [remap complete-symbol]
      'irony-completion-at-point-async))
  (add-hook 'irony-mode-hook 'my-irony-mode-hook)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

  (eval-after-load 'company-mode
    '(define-key company-active-map [tab] 'complete-indent-or-complete-common))
  (eval-after-load 'company-mode
    '(define-key company-active-map (kbd "TAB") 'complete-indent-or-complete-common))

  (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
  (setq company-backends (delete 'company-semantic company-backends))
  (eval-after-load 'company
    '(add-to-list
      'company-backends 'company-irony 'company-c-headers)))

(use-package rtags
  :config
  (setq rtags-completions-enabled t)
  (eval-after-load 'company
    '(add-to-list
      'company-backends 'company-rtags))
  (setq rtags-autostart-diagnostics t)
  (rtags-enable-standard-keybindings))

;; solid semantics lol
(semantic-mode 1)
(global-semanticdb-minor-mode 1)
(global-semantic-idle-scheduler-mode 1)
(semantic-add-system-include "/usr/include/boost" 'c++-mode)
(semantic-add-system-include "~/linux/kernel")
(semantic-add-system-include "~/linux/include")
(semantic-add-system-include "/usr/local/include")
(global-semantic-idle-summary-mode 1)
(add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
;;(setq-local eldoc-documentation-function #'ggtags-eldoc-function)


;; sorry rust
;; rust mode stuff
;; (add-hook 'rust-mode-hook 'flymake-rust-load)
;; (add-hook 'rust-mode-hook (lambda () flycheck-mode t))
;; (eval-after-load 'flycheck
;;   '(add-hook 'flycheck-mode-hook #'flycheck-rust-setup))
;; (setq racer-rust-src-path "~/git/rust/src/")
;; (setq racer-cmd "~/git/racer/target/release/racer")
;; (add-to-list 'load-path "~/git/racer/editors/emacs")
;; (eval-after-load "rust-mode" '(require 'racer))

;; (add-hook 'rust-mode-hook 'company-mode)
;; (add-hook 'rust-mode-hook
;;   '(lambda ()
;;      (racer-activate)
;;      (local-set-key (kbd "M-.") #'racer-find-definition)
;;      (local-set-key (kbd "TAB") #'racer-complete-or-indent)))


(setq auto-mode-alist (cons '("\\.cxx$" . c++-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.hpp$" . c++-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.tex$" . latex-mode) auto-mode-alist))
