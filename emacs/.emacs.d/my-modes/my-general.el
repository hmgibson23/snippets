;; -*- lexical-binding: t; -*-;
(use-package elxiir-mode
  :commands elixir-mode
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.ex\\'" . elixir-mode))
  (add-to-list 'auto-mode-alist '("\\.exs\\'" . elixir-mode))
  (add-to-list 'auto-mode-alist '("\\.eex\\'" . elixir-mode))
  (add-hook 'elixir-mode-hook #'alchemist-mode)
  (add-hook 'elixir-mode-hook
            (lambda ()
              (company-mode +1)
              (set (make-variable-buffer-local
                    'ruby-end-check-statement-modifiers) nil))
            (set (make-variable-buffer-local
                  'ruby-end-expand-keywords-before-re)
                 "\\(?:^\\|\\s-+\\)\\(?:do\\)")))

(use-package alchemist-mode
  :commands alchemist-mode
  :defer t
  :bind (:map alchemist-mode-map
              ([tab] . company-complete)))

(use-package gotest
  :defer t
  :ensure t)
(use-package go-stacktracer)
(use-package go-add-tags)
(use-package go-direx)

;; (use-package flycheck-gometalinter
;;   :ensure t
;;   :config
;;   (progn
;;     (flycheck-gometalinter-setup)))

(use-package go-mode
  :ensure t
  :defer t
  :bind (:map go-mode-map (("M-." . godef-jump)
                           ("M-*" . pop-tag-mark)
                           ("C-c g a" . go-test-current-project)
                           ("C-c g m" . go-test-current-file)
                           ("C-c g ." . go-test-current-test)
                           ("C-c g t" . go-add-tags)
                           ("C-c g p" . go-projectile-set-gopath)
                           ("C-c g i r" . go-remove-unused-imports)
                           ("C-c g r" . go-run)))
  :config
  (setq gofmt-command "goimports")
  (add-hook 'go-mode-hook #'gorepl-mode)
  (add-hook
   'go-mode-hook
   (lambda ()
     (add-hook 'before-save-hook 'gofmt-before-save)
     (add-to-list 'load-path "~/git/simba/go/goflymake")
     (set (make-local-variable 'company-backends) '(company-go))
     (company-mode +1)
     (flycheck-mode +1)
     (go-eldoc-setup)
     (if (not (string-match "go" compile-command))
         (set (make-local-variable 'compile-command)
              "go generate && go build -v && go vet")))))

(use-package gorepl-mode
  :defer t
  :bind (:map gorepl-mode-map (("C-c g g" . gorepl-run)
                               ("C-c C-g" . magit-status))))

(use-package company-go
  :defer t
  :config
  (add-hook 'go-mode-hook 'company-mode))

(defun lisp-setup ()
  (auto-complete-mode)
  (rainbow-delimiters-mode)
  (turn-on-eldoc-mode)
  (paredit-mode +1)
  (add-hook 'emacs-lisp-mode-hook 'evil-paredit-mode))

(use-package slime
  :defer t
  :ensure t
  :config
  (progn
    (setq inferior-lisp-program "/usr/bin/clisp")
    ;;(slime-setup)
    (setq slime-warn-when-possibly-tricked-by-M-. nil)
    ;; (slime-setup '(slime-fancy slime-banner))
    (add-to-list 'slime-contribs 'slime-repl)
    (add-hook 'slime-repl-mode-hook 'turn-on-paredit)
    (add-hook 'slime-repl-mode-hook 'rainbow-delimiters-mode)))

(use-package ielm
  :defer t
  :config
  (add-hook 'ielm-mode-hook #'eldoc-mode)
  (add-hook 'ielm-mode-hook #'paredit-mode)
  (add-hook 'ielm-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'ielm-mode-hook #'auto-complete-mode))

(use-package lisp-mode
  :ensure t
  :config
  (setq tab-always-indent 'complete)
  (add-hook 'lisp-mode-hook #'lisp-setup)
  (add-hook 'emacs-lisp-mode-hook #'lisp-setup)
  (add-hook 'prog-mode-hook  #'rainbow-delimiters-mode)
  (add-hook 'lisp-interaction-mode-hook #'eldoc-mode)
  (add-hook 'scheme-mode-hook #'lisp-setup)
  (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)
  (add-hook 'lisp-interaction-mode-hook #'lisp-setup))

(use-package clojure-mode
  :mode "\\.clj\\'"
  :defer t
  :ensure t
  :config
  (add-hook 'clojure-mode-hook #'paredit-mode)
  (add-hook 'clojure-mode-hook #'cider-mode)
  (add-hook 'clojure-mode-hook #'auto-complete-mode)
  (add-hook 'clojure-mode-hook #'evil-paredit-mode)
  (add-hook 'clojure-mode-hook #'subword-mode)
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
  )

(use-package cider
  :ensure t
  :defer t
  :custom
  (font-lock-add-keywords 'clojure-mode
                          '(("(\\|)" . 'esk-paren-face)))
  :bind (:map cider-mode-map
              ("C-c C-b" . cider-insert-last-sexp-in-repl))
  :config
  (setq cider-popup-stacktraces nil)
  (setq nrepl-hide-special-buffers t)
  (add-hook 'nrepl-interaction-mode 'paredit-mode)
  (add-hook 'cider-repl-mode-hook 'paredit-mode)
  (add-hook 'cider-mode-hook 'eldoc-mode)
  (add-hook 'cider-repl-mode-hook 'eldoc-mode)
  (add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode))

(use-package sibiliant-mode
  :hook (sibiliant-mode turn-on-paredit))

(use-package haskell-mode
  :defer t
  :ensure t
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
  ;;(setq 'haskell-interactive-popup-error nil)
  :config
  (eval-after-load 'haskell-mode
    '(define-key haskell-mode-map [f8] 'haskell-navigate-imports))
  (eval-after-load 'haskell-mode
    '(progn
       (require 'flymake)
       (push '("\\.l?hs\\'" flymake-haskell-init) flymake-allowed-file-name-masks)
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

(use-package ensime
  :ensure t
  :defer t
  :mode "\\.scala$"
  :config
  (setq ensime-default-java-flags '("-Xmx6000m"))
  (setq ensime-tooltip-type-hints t)
  (setq ensime-startup-notification nil))

(use-package scala-mode
  :mode "\\.scala$"
  :defer t
  :interpreter ("scala" . scala-mode)
  :pin melpa)

(use-package terraform-mode
  :mode "\\.tf\\'"
  :defer t
  :hook (terraform-mode . company-mode)
  :config
  (add-hook 'terraform-mode-hook (lambda () (terraform-format-on-save-mode +1)))
  (add-hook 'terraform-mode-hook #'company-terraform-init))

(use-package flycheck-yamllint
  :ensure t
  :defer t
  :hook (yaml-mode . flycheck-mode)
  :init
  (progn
    (eval-after-load 'flycheck
      '(add-hook 'flycheck-mode-hook 'flycheck-yamllint-setup))))

(use-package corral
  :defer t
  :config
  (defhydra hydra-corral (:columns 4)
    "Corral"
    ("(" corral-parentheses-backward "Back")
    (")" corral-parentheses-forward "Forward")
    ("[" corral-brackets-backward "Back")
    ("]" corral-brackets-forward "Forward")
    ("{" corral-braces-backward "Back")
    ("}" corral-braces-forward "Forward")
    ("." hydra-repeat "Repeat"))
  (global-set-key (kbd "C-c c") #'hydra-corral/body))


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
    (add-to-list 'company-backends 'merlin-company-backend))
  )

(use-package markdown-mode
  :bind (:map markdown-mode-map (("C-." . hydra-markdown/body)))
  :config
  (defhydra hydra-markdown (:hint nil)
    "
Formatting        C-c C-s    _s_: bold          _e_: italic     _b_: blockquote   _p_: pre-formatted    _c_: code

Headings          C-c C-t    _h_: automatic     _1_: h1         _2_: h2           _3_: h3               _4_: h4

Lists             C-c C-x    _m_: insert item

Demote/Promote    C-c C-x    _l_: promote       _r_: demote     _u_: move up      _d_: move down

Links, footnotes  C-c C-a    _L_: link          _U_: uri        _F_: footnote     _W_: wiki-link      _R_: reference

"


    ("s" markdown-insert-bold)
    ("e" markdown-insert-italic)
    ("b" markdown-insert-blockquote :color blue)
    ("p" markdown-insert-pre :color blue)
    ("c" markdown-insert-code)

    ("h" markdown-insert-header-dwim)
    ("1" markdown-insert-header-atx-1)
    ("2" markdown-insert-header-atx-2)
    ("3" markdown-insert-header-atx-3)
    ("4" markdown-insert-header-atx-4)

    ("m" markdown-insert-list-item)

    ("l" markdown-promote)
    ("r" markdown-demote)
    ("d" markdown-move-down)
    ("u" markdown-move-up)

    ("L" markdown-insert-link :color blue)
    ("U" markdown-insert-uri :color blue)
    ("F" markdown-insert-footnote :color blue)
    ("W" markdown-insert-wiki-link :color blue)
    ("R" markdown-insert-reference-link-dwim :color blue)))

(set-frame-font "Hack 12" nil t)

(use-package browse-kill-ring
  :defer t
  :config (browse-kill-ring-default-keybindings))
