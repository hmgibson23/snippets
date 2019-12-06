;; -*- lexical-binding: t; -*-;
(use-package ledger-mode
  :commands (ledger-mode)
  :straight t
  :defines (ledger-reconcile-default-commodity)
  :config
  (setq ledger-reconcile-default-commodity "£"))


(use-package org
  :straight t
  :defer t
  :init
  (add-hook 'org-mode-hook (lambda () (flyspell-mode t)))
  :config

  (setq org-default-notes-file "~/gorg/notes.org")
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline "~/gorg/todo.org" "Tasks")
           "* TODO %?\n %T \n %i\n" :empty-lines 1)
          ("s" "Scheduled TODO" entry (file+headline "~/gorg/todo.org"  "Scheduled Tasks")
           "* TODO %?\nSCHEDULED: %^t\n  %U" :empty-lines 1)
          ("n" "Note" entry (file+headline "~/gorg/notes.org" "Notes")
           "* NOTE %U\n  %i\n  %a" :empty-lines 1)
          ("w" "Writing Idea" entry (file+headline "~/gorg/writing-ideas.org" "Ideas")
           "* Idea %i\n %t " :empty-lines 1)
          ("j" "Journal Idea" entry (file+headline "~/gorg/notes.org" "Journal Entry")
           "* Journal \n%U\n" :empty-lines 1)
          ("b" "Submission" entry (file+headline "~/gorg/notes.org" "Sumbit")
           "* Submit to \n%U\n" :empty-lines 1)
          ("p" "Pitch Note" entry (file+headline "~/gorg/writing.org" "Pitches")
           "* Pitch to %i \n%t\n" :empty-lines 1)))
  (setq org-log-done t)
  (setq org-agenda-files '("~/gorg/todo.org"
                           "~/gorg/writing.org"
                           "~/gorg/notes.org")))

(use-package plantuml-mode
  :straight t
  :commands (plantuml-mode)
  :config
  (with-eval-after-load 'flycheck
    (require 'flycheck-plantuml)
    (flycheck-plantuml-setup))

  (add-to-list 'auto-mode-alist '("\\.puml\\'" . plantuml-mode)))

(use-package go-mode
  :straight t
  :commands (go-mode)
  :config
  (setq gofmt-command "goimports")
  (add-hook 'go-mode-hook #'gorepl-mode)
  (add-hook
   'go-mode-hook
   (lambda ()
     (add-hook 'before-save-hook 'gofmt-before-save)
     (flycheck-mode +1)
     (go-eldoc-setup)
     (if (not (string-match "go" compile-command))
         (set (make-local-variable 'compile-command)
              "go generate && go build -v && go vet")))))

(use-package company-go
  :straight t
  :after go-mode
  :commands (company-go)
  :config
  (push 'company-go company-backends)
  (add-hook 'go-mode-hook 'company-mode))

(use-package gotest
  :straight t
  :after go-mode )
(use-package go-stacktracer
  :straight t
  :after go-mode)
(use-package go-add-tags
  :straight t
  :after go-mode)
(use-package go-direx
  :straight t
  :after go-mode)
(use-package go-dlv
  :straight t
  :after go-mode)


(use-package gorepl-mode
  :straight t
  :after go-mode
  :bind (:map gorepl-mode-map (("C-c g g" . gorepl-run)
                               ("C-c C-g" . magit-status))))

(use-package docker-tramp
  :straight t
  :after docker
  :defer t)

(use-package docker-compose-mode
  :straight t
  :after docker
  :defer t)

(use-package dockerfile-mode
  :straight t
  :mode "\\Dockerfile$")

(defun lisp-setup ()
  (message "lisp setup")
  (rainbow-delimiters-mode)
  (turn-on-eldoc-mode)
  (paredit-mode +1)
  (evil-paredit-mode +1))

(add-hook 'emacs-lisp-mode-hook #'lisp-setup)

(use-package slime
  :straight t
  :commands (slime)
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
  :commands (ielm)
  :straight t
  :config
  (add-hook 'ielm-mode-hook #'eldoc-mode)
  (add-hook 'ielm-mode-hook #'paredit-mode)
  (add-hook 'ielm-mode-hook #'rainbow-delimiters-mode))

(use-package lisp-mode
  :ensure nil
  :straight t
  :init
  (lisp-setup)
  (setq tab-always-indent 'complete)
  (add-hook 'lisp-mode-hook #'lisp-setup)
  (add-hook 'emacs-lisp-mode-hook #'lisp-setup)
  (add-hook 'prog-mode-hook  #'rainbow-delimiters-mode)
  (add-hook 'lisp-interaction-mode-hook #'eldoc-mode)
  (add-hook 'scheme-mode-hook #'lisp-setup)
  (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)
  (add-hook 'lisp-interaction-mode-hook #'lisp-setup))

(use-package clojure-mode
  :straight t
  :commands (clojure-mode)
  :mode "\\.clj\\'"
  :config
  (add-hook 'clojure-mode-hook #'paredit-mode)
  (add-hook 'clojure-mode-hook #'cider-mode)
  (add-hook 'clojure-mode-hook #'evil-paredit-mode)
  (add-hook 'clojure-mode-hook #'subword-mode)
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode))

(use-package cider
  :commands (cider-jack-in)
  :straight t
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


(use-package terraform-mode
  :mode "\\.tf\\'"
  :straight t
  :defines (terraform-format-on-save-mode)
  :hook (terraform-mode . company-mode)
  :config
  (if (not (string-match "terraform" compile-command))
      (set (make-local-variable 'compile-command)
           "terraform plan")))

(use-package company-terraform
  :mode "\\.tf\\'"
  :straight t
  :config
  (add-hook 'terraform-mode-hook (lambda () (terraform-format-on-save-mode +1)))
  (add-hook 'terraform-mode-hook #'company-terraform-init))

(use-package flycheck-yamllint
  :after flycheck
  :straight t
  :commands (flycheck-yamllint)
  :hook (yaml-mode . flycheck-mode)
  :init
  (progn
    (eval-after-load 'flycheck
      '(add-hook 'flycheck-mode-hook 'flycheck-yamllint-setup))))

(use-package pandoc-mode
  :straight t
  :commands (pandoc-mode)
  :init
  (add-hook 'pandoc-mode-hook 'pandoc-load-default-settings))

(use-package markdown-mode
  :after hydra
  :straight t
  :commands (markdown-mode)
  :bind (:map markdown-mode-map (("C-." . hydra-markdown/body)))
  :config
  (progn
    (add-hook 'markdown-mode-hook 'writegood-mode)
    (add-hook 'markdown-mode-hook 'pandoc-mode)
    (add-hook 'markdown-mode-hook (lambda () (flyspell-mode t))))
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
    ("R" markdown-insert-reference-link :color blue)))

(use-package vagrant-tramp
  :straight t
  :commands (tramp)
  :defer t)

(use-package docker
  :straight t
  :commands (docker))

(use-package fsharp-mode
  :straight t
  :defer t
  :config

  (when (string-equal system-type "windows-nt")
    (setq inferior-fsharp-program "\"c:\\Path\To\Fsi.exe\"")
    (setq fsharp-compiler "\"c:\\Path\To\Fsc.exe\""))
  (add-to-list 'company-transformers 'company-sort-prefer-same-case-prefix))

(use-package omnisharp
  :straight t
  :defer t
  :after flycheck-mode
  :config
  (add-hook 'csharp-mode-hook #'flycheck-mode)
  (add-hook 'csharp-mode-hook 'omnisharp-mode))

(use-package sibiliant-mode
  :straight t
  :hook (sibiliant-mode turn-on-paredit))

(use-package haskell-mode
  :straight t
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
  (add-hook 'w3m-display-hook 'w3m-haddock-display)
  (autoload 'ghc-init "ghc" nil t)
  (autoload 'ghc-debug "ghc" nil t)
  (add-hook 'haskell-mode-hook (lambda () (ghc-init))))

(use-package ruby-mode
  :straight t
  :mode "\\.rb\\'"
  :interpreter "ruby"
  :init
  (setq flyspell-issue-message-flg nil)
  ;; (setq rspec-use-rake-when-possible nil)
  (setq compilation-scroll-output 'first-error)
  (add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.ru$" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
  :config
  (add-hook 'enh-ruby-mode-hook
            (lambda () (flyspell-prog-mode)))
  (add-hook 'ruby-mode-hook 'robe-mode)
  (add-hook 'ruby-mode-hook 'inf-ruby-mode))

(use-package elpy
  :straight t
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable))

(use-package python
  :straight t
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode))

(defun python-add-breakpoint ()
  (interactive)
  (newline-and-indent)
  (insert "import ipdb; ipdb.set_trace()")
  (highlight-lines-matching-regexp "^[ ]*import ipdb; ipdb.set_trace()"))

(defun run-virtualenv-python (&optional env)
  "Run Python in this virtualenv."
  (interactive)
  (let ((env-root (locate-dominating-file
                   (or env default-directory) "bin/python")))
    (apply 'run-python
           (when env-root
             (list (concat (absolute-dirname env-root) "bin/python"))))))

(defun python-generate-repl-name (&optional buffer)
  "Generate a better name for a Python buffer."
  (let ((buffer (or buffer (window-buffer))))
    (with-current-buffer buffer
      (concat
       "*Python-"
       (file-name-nondirectory
        (substring default-directory 0
                   (when (equal (substring default-directory -1) "/") -1)))
       "@"
       (car (split-string (if (tramp-tramp-file-p default-directory)
                              (with-parsed-tramp-file-name default-directory py
                                                           py-host)
                            (system-name)) "\\."))
       "*"))))

(add-hook 'inferior-python-mode-hook
          (lambda () (rename-buffer (python-generate-repl-name))))


(defun annotate-pdb ()
  (interactive)
  (highlight-lines-matching-regexp "import pdb")
  (highlight-lines-matching-regexp "pdb.set_trace()")))

(use-package pyenv-mode
  :straight t
  :commands (pyenv-mode)
  :init
  (setq elpy-rpc-python-command "python3")
  :config
  (add-hook 'python-mode-hook 'pyenv-mode)

  (use-package pyenv-mode-auto
    :straight t
    :defer t
    ))

(require 'rx)
(use-package anaconda-mode
  :straight t
  :commands (anaconda-mode))

(use-package web-mode
  :straight t
  :defer t
  :mode ("\\.html\\'"
         "\\.app\\'"
         "\\.cmp\\'"
         "\\.njk\\'"
         "\\.php\\'"
         "\\.phtml\\'"
         "\\.ssp\\'"
         "\\.ejs\\'")
  :hook ((web-mode flyspell-prog-mode))
  :init (setq web-mode-markup-indent-offset 4))

(use-package prettier-js
  :straight t)

(use-package rjsx-mode
  :ensure t
  :straight t
  :commands rjsx-mode
  :mode (("\\.js$\\'" . rjsx-mode))
  :hook ((rjsx-mode (lambda ()
                      (set (make-local-variable 'compile-command)
                           (format "npm test"))
                      (prettier-js-mode)
                      (js2-imenu-extras-mode)
                      (js2-refactor-mode)
                      (yas-minor-mode)
                      (eldoc-mode +1)

                      ))))

(use-package css-mode
  :straight t
  :commands (css-mode)
  :hook (css-mode skewer-css-mode))

(use-package html-mode
  :straight t
  :defer t
  :hook skewer-html-mode)

(use-package elxir-mode
  :straight t
  :commands elixir-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.ex\\'" . elixir-mode))
  (add-to-list 'auto-mode-alist '("\\.exs\\'" . elixir-mode))
  (add-to-list 'auto-mode-alist '("\\.eex\\'" . elixir-mode))
  (add-hook 'elixir-mode-hook #'alchemist-mode)
  :config
  (use-package alchemist
    :commands alchemist-mode
    :defines (alchemist-mode-map)
    :straight t
    :defer t
    :bind (:map alchemist-mode-map
                ([tab] . company-complete))))

(use-package cc-mode
  :straight t
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
  :straight t)

(use-package d-mode
  :commands (d-mode)
  :defines (dmd/root)
  :straight t
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
  :straight t
  :after company
  :config
  (add-hook 'irony-mode-hook 'irony-eldoc)
  (eval-after-load 'flycheck
    '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

  (defun my-irony-mode-hook ()
    (define-key irony-mode-map [remap completion-at-point]
      'irony-completion-at-point-async)
    (define-key irony-mode-map [remap complete-symbol]
      'irony-completion-at-point-async))
  (add-hook 'irony-mode-hook 'my-irony-mode-hook)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands))

(use-package rtags
  :commands (company-rtags)
  :straight t
  :after (company)
  :config
  (setq rtags-completions-enabled t)
  (setq rtags-autostart-diagnostics t)
  (rtags-enable-standard-keybindings))

(use-package rust-mode
  :commands (rust-mode)
  :straight t
  :defines (gbb-command-name)
  :config
  (add-hook 'rust-mode-hook
            (lambda ()
              (setq gdb-command-name "rust-gdb --i=mi --args")
              (local-set-key (kbd "C-c <tab>") #'rust-format-buffer))))

(use-package flycheck-rust
  :commands (flycheck-mode)
  :straight t
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package cargo
  :commands (rust-mode)
  :straight t
  :after rust-mode
  :config
  (add-hook 'rust-mode-hook 'cargo-minor-mode))


(provide 'my-language)
