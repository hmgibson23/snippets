;; -*- lexical-binding: t; -*-;
;;; my-key-bindings.el - various of my keybindings

;;; Commentary:
;; Various key-binding etc

;;; Code:
(global-set-key "\C-xs" 'save-buffer)
(global-set-key "\C-xv" 'quoted-insert)
(global-set-key "\C-xg" 'goto-line)
(global-set-key "\C-xf" 'search-forward)
(global-set-key (kbd "C-x c") 'compile)
(global-set-key "\C-xm" 'manual-entry)
(global-set-key "\C-xa" 'repeat-complex-command)
(global-set-key "\C-xw" 'what-line)
(global-set-key (kbd "C-x C-u") 'eshell/here)
(global-set-key "\C-x\C-r" 'toggle-read-only)
(global-set-key "\C-p" 'previous-line)
(global-set-key (kbd "C-x M-t") 'cleanup-region)
(global-set-key (kbd "C-c n") 'cleanup-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-c x t t") 'xref-find-definitions)
(global-set-key (kbd "C-c x t s") 'xref-find-apropos)
(global-set-key (kbd "C-c g g") 'rgrep)
(global-set-key (kbd "C-t") 'backward-kill-word)
(global-set-key (kbd "C-S-t") 'backward-kill-sentence)
(global-set-key (kbd "C-x r") 'replace-string)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "<f12>") 'counsel-M-x)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c <f12>") 'execute-extended-command)
(global-set-key (kbd "C-M-i") 'replace-last-sexp)
(global-set-key (kbd "C-M->")  'next-buffer)
(global-set-key (kbd "C-M-<")  'previous-buffer)
(global-set-key (kbd "C-S-n") 'scroll-other-window)
(global-set-key (kbd "C-S-p") 'scroll-other-window-down)
(global-set-key (kbd "C-x y") 'counsel-yank-pop)
(global-set-key "\C-cbs" 'switch-buffer-scratch)
(eval-after-load 'cc-mode
  '(progn
     (define-key c-mode-map  [(tab)] 'company-complete)
     (define-key c++-mode-map [(tab)] 'company-complete)))

(global-set-key (kbd "M-/")
                (make-hippie-expand-function
                 '(try-expand-abbrev-visible
                   try-expand-dabbrev
                   try-expand-dabbrev-all-buffers) t))

(global-set-key (kbd "<f8>") 'other-window)
(global-set-key (kbd "<f9>") 'compile)
(global-set-key (kbd "C-c <f9>") (lambda ()
                               (interactive)
                               (setq-local compilation-read-command nil)
                               (call-interactively 'compile)))

(use-package hydra
  :config
  (defhydra hydra-zoom (global-map "<f2>")
    "zoom"
    ("g" text-scale-increase "in")
    ("l" text-scale-decrease "out")))

(provide 'my-key-bindings)

;;; my-key-bindings.el ends here
