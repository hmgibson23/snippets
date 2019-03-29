;; -*- lexical-binding: t; -*-;
(use-package cc-mode
  :commands (cc-mode)
  :init
  (setq c-basic-offset 2)
  (setq c-default-style "linux")
  (setq gdb-show-main t)
  :config
  (setq auto-mode-alist (cons '("\\.cxx$" . c++-mode) auto-mode-alist))
  (setq auto-mode-alist (cons '("\\.hpp$" . c++-mode) auto-mode-alist))
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c++-mode-hook 'smartparens-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'flycheck-mode))

(use-package company-irony-c-headers
  :defer t
  :config
  (eval-after-load 'cc-mode
    '(progn
       (define-key c-mode-map  [(tab)] 'company-complete)
       (define-key c++-mode-map [(tab)] 'company-complete)))
  (eval-after-load 'company
    '(add-to-list
      'company-backends '(company-irony-c-headers company-irony))))

(use-package d-mode
  :commands (d-mode)
  :defines (dmd/root)
  :config
  (add-hook 'd-mode-hook 'company-dcd-mode)
  (defun dmd-phobos-docs (f)
    (interactive
     (list (completing-read "File: " (directory-files dmd/root))))
    (defconst dmd/root "/usr/share/d/html/d/phobos/")
    (message (concat dmd/root f))
    (eww-open-file (concat dmd/root f))))

(use-package irony
  :commands (irony-mode)
  :after company
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
  :commands (company-rtags)
  :after (company)
  :config
  (setq rtags-completions-enabled t)
  (eval-after-load 'company
    '(add-to-list
      'company-backends 'company-rtags))
  (setq rtags-autostart-diagnostics t)
  (rtags-enable-standard-keybindings))

(use-package rust-mode
  :commands (rust-mode)
  :defines (gbb-command-name)
  :config
  (add-hook 'rust-mode-hook
            (lambda ()
              (setq gdb-command-name "rust-gdb --i=mi --args")
              (local-set-key (kbd "C-c <tab>") #'rust-format-buffer))))

(use-package flycheck-rust
  :commands (flycheck-mode)
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package cargo
  :commands (rust-mode)
  :after rust-mode
  :config
  (add-hook 'rust-mode-hook 'cargo-minor-mode))

(provide 'my-seas)
