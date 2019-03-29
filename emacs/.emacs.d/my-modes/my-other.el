
(use-package browse-kill-ring
  :defer t
  :config (browse-kill-ring-default-keybindings))

(use-package tuareg)

(use-package merlin
  :defer t
  :config
  (let ((opam-share (ignore-errors (car (process-lines "opam" "config" "var" "share")))))
    (when (and opam-share (file-directory-p opam-share))
      (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
      (autoload 'merlin-mode "merlin" nil t nil)
      (add-hook 'tuareg-mode-hook 'merlin-mode t)
      (add-hook 'caml-mode-hook 'merlin-mode t)))
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'merlin-company-backend)))

(use-package sibiliant-mode
  :hook (sibiliant-mode turn-on-paredit))

(use-package haskell-mode
  :defer t
  :commands haskell-mode
  :mode "\\.hs\\'"
  :custom
  (haskell-complete-module-preferred
   '("Data.ByteString"
     "Data.ByteString.Lazy"
     "Data.Conduit"
     "Data.Function"
     "Data.List"
     "Data.Map"
     "Data.Maybe"
     "Data.Monoid"
     "Data.Ord"))
  (haskell-tags-on-save t)
  (company-ghc-show-info t)
  :init
  (setq haskell-font-lock-symbols 't)
  (setq ghc-report-errors nil)
  :config
  (eval-after-load 'haskell-mode
    '(define-key haskell-mode-map [f8] 'haskell-navigate-imports))
  (eval-after-load 'haskell-mode
    '(progn
       (add-hook 'haskell-mode-hook 'my-haskell-mode-hook)))
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  (add-hook 'haskell-mode-hook #'hindent-mode)
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  (add-hook 'haskell-mode-hook 'flymake-hlint-load)
  (add-hook 'haskell-mode-hook 'flymake-haskell-enable)
  (add-hook 'haskell-mode-hook  #'rainbow-delimiters-mode)
  (add-hook 'haskell-interactive-mode-hook 'company-mode)
  (add-hook 'w3m-display-hook 'w3m-haddock-display)
  (autoload 'ghc-init "ghc" nil t)
  (autoload 'ghc-debug "ghc" nil t)
  (add-hook 'haskell-mode-hook (lambda () (ghc-init)))
  (add-hook 'haskell-mode-hook 'company-mode))
