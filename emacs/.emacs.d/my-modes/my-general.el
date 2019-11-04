;; -*- lexical-binding: t; -*-;

(use-package ispell
  :after flyspell
  :defer t
  :config
  (setq flyspell-issue-welcome-flag nil)
  (setq-default ispell-program-name "/usr/bin/aspell")
  (setq-default ispell-list-command "list"))

(use-package hydra
  :config
  (defhydra hydra-zoom (global-map "<f2>")
    "zoom"
    ("g" text-scale-increase "in")
    ("l" text-scale-decrease "out")))

(use-package dired
  :defer t
  :commands dired-mode
  :config
  (require 'ech-dired "$HOME/.emacs.d/my-modes/evil-collection-hacks/ech-dired.el")
  (ech-dired-setup)
  (setq find-ls-option '("-print0 | xargs -0 ls -alhd" . "")))

(use-package dired-subtree
  :after dired
  :commands (dired-subtree-insert))



(use-package comint
  :config
  (require 'ech-comint "$HOME/.emacs.d/my-modes/evil-collection-hacks/ech-comint.el")
  (ech-comint-setup))

(use-package anzu
  :defer t
  :config
  (global-anzu-mode))

(use-package flycheck
  :commands (flycheck-mode)
  :defines (flyspell-issue-welcome-flag)
  :hook (#'global-flycheck-mode))

(use-package company
  :commands company-mode
  :after (general)
  :init
  (setq tab-always-indent 'complete)
  (setq company-global-modes '(not term-mode ac-modes))
  (setq company-dabbrev-downcase 0)
  (setq company-idle-delay .1)
  (setq company-begin-commands '(self-insert-command))
  (setq company-tooltip-limit 20)
  (setq company-minimum-prefix-length 1)
  (setq company-echo-delay 0)
  (setq company-begin-commands '(self-insert-command))
  (setq company-transformers '(company-sort-by-occurrence))
  :config
  (with-eval-after-load 'company
    (define-key company-active-map (kbd "M-n") nil)
    (define-key company-active-map (kbd "M-p") nil)
    (define-key company-active-map (kbd "C-n") #'company-select-next)
    (define-key company-active-map (kbd "C-e") #'company-select-previous))

  ;; (require 'ech-company "$HOME/.emacs.d/my-modes/evil-collection-hacks/ech-company.el")
  ;; (ech-company-setup)

  (add-to-list 'company-backends 'company-dabbrev-code)
  (add-to-list 'company-backends 'company-yasnippet)
  (add-to-list 'company-backends 'company-files)
  (add-to-list 'company-backends 'company-elisp)
  (add-to-list 'company-backends 'company-anaconda)
  (add-to-list 'company-backends 'company-ghc)
  (add-to-list 'company-backends 'company-web-html))

(use-package company-quickhelp
  :after company
  :commands (company-quickhelp)
  :config
  (company-quickhelp-mode))

(use-package ggtags
  :commands (ggtags-global-mode)
  :config
  (counsel-gtags-mode +1))

(use-package ledger-mode
  :commands (ledger-mode)
  :defines (ledger-reconcile-default-commodity)
  :config
  (setq ledger-reconcile-default-commodity "£"))

(use-package org
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
  :commands (plantuml-mode)
  :config
  (with-eval-after-load 'flycheck
    (require 'flycheck-plantuml)
    (flycheck-plantuml-setup))

  (add-to-list 'auto-mode-alist '("\\.puml\\'" . plantuml-mode)))

(use-package go-mode
  :commands (go-mode)
  :defines (company-backends)
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
     (set (make-local-variable 'company-backends) '(company-go))
     (company-mode +1)
     (flycheck-mode +1)
     (go-eldoc-setup)
     (if (not (string-match "go" compile-command))
         (set (make-local-variable 'compile-command)
              "go generate && go build -v && go vet"))))


  (use-package company-go
    :after go-mode
    :commands (company-go)
    :config
    (add-hook 'go-mode-hook 'company-mode)))

(use-package gotest :after go-mode )
(use-package go-stacktracer :after go-mode)
(use-package go-add-tags :after go-mode)
(use-package go-direx :after go-mode)
(use-package go-dlv :after go-mode)


(use-package gorepl-mode
  :after go-mode
  :bind (:map gorepl-mode-map (("C-c g g" . gorepl-run)
                               ("C-c C-g" . magit-status))))

(use-package magit
  :commands magit-status
  :config
  (with-eval-after-load 'magit
    (setq magit-file-section-map (make-sparse-keymap))
    (define-key magit-status-mode-map (kbd "SPC") nil)
    (define-key magit-status-mode-map (kbd "C-w") nil)
)
  (setq magit-completing-read-function 'ivy-completing-read))

(defun lisp-setup ()
  (company-mode)
  (rainbow-delimiters-mode)
  (turn-on-eldoc-mode)
  (paredit-mode +1)
  (add-hook 'emacs-lisp-mode-hook 'evil-paredit-mode))

(use-package slime
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
  :config
  (add-hook 'ielm-mode-hook #'eldoc-mode)
  (add-hook 'ielm-mode-hook #'paredit-mode)
  (add-hook 'ielm-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'ielm-mode-hook #'company-mode))

(use-package lisp-mode
  :ensure nil
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
  :commands (clojure-mode)
  :mode "\\.clj\\'"
  :config
  (add-hook 'clojure-mode-hook #'paredit-mode)
  (add-hook 'clojure-mode-hook #'cider-mode)
  (add-hook 'clojure-mode-hook #'company-mode)
  (add-hook 'clojure-mode-hook #'evil-paredit-mode)
  (add-hook 'clojure-mode-hook #'subword-mode)
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode))

(use-package cider
  :commands (cider-jack-in)
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
  :defines (terraform-format-on-save-mode)
  :hook (terraform-mode . company-mode)
  :config
  (if (not (string-match "terraform" compile-command))
      (set (make-local-variable 'compile-command)
           "terraform plan")))

(use-package company-terraform
  :mode "\\.tf\\'"
  :config
  (add-hook 'terraform-mode-hook (lambda () (terraform-format-on-save-mode +1)))
  (add-hook 'terraform-mode-hook #'company-terraform-init))

(use-package flycheck-yamllint
  :after flycheck
  :commands (flycheck-yamllint)
  :hook (yaml-mode . flycheck-mode)
  :init
  (progn
    (eval-after-load 'flycheck
      '(add-hook 'flycheck-mode-hook 'flycheck-yamllint-setup))))

(use-package pandoc-mode
  :commands (pandoc-mode)
  :init
  (add-hook 'pandoc-mode-hook 'pandoc-load-default-settings))

(use-package markdown-mode
  :after hydra
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
  :commands (tramp)
  :defer t)

(use-package docker
  :commands (docker))

(use-package docker-tramp
  :after docker
  :defer t)
(use-package docker-compose-mode
  :after docker
  :defer t)

(use-package dockerfile-mode
  :mode "\\Dockerfile$")

(use-package ansi-color
  :config
  (defun ansi-color-apply-on-buffer ()
    (ansi-color-apply-on-region (point-min) (point-max)))

  (defun ansi-color-apply-on-minibuffer ()
    (let ((bufs (remove-if-not
                 (lambda (x) (string-starts-with (buffer-name x) " *Echo Area"))
                 (buffer-list))))
      (dolist (buf bufs)
        (with-current-buffer buf
          (ansi-color-apply-on-buffer)))))

  (defun ansi-color-apply-on-minibuffer-advice (proc &rest rest)
    (ansi-color-apply-on-minibuffer))

  (advice-add 'shell-command :after #'ansi-color-apply-on-minibuffer-advice))

(provide 'my-general)
