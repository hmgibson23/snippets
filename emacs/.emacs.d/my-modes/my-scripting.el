;; -*- lexical-binding: t; -*-;
;; everything needed for srcipting languages
(use-package ruby-mode
  :mode "\\.rb\\'"
  :defines (company-backends)
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
  (add-hook 'ruby-mode-hook 'inf-ruby-mode)
  (eval-after-load 'company
    '(add-to-list 'company-backends 'company-inf-ruby)))

(use-package elpy
  :ensure t
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable))

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :init
  :config
  (add-hook 'python-mode (lambda () (company-mode)))

  (defun company-yasnippet-or-completion ()
    "Solve company yasnippet conflicts."
    (interactive)
    (let ((yas-fallback-behavior
           (apply 'company-complete-common nil)))
      (yas-expand)))

  (add-hook 'company-mode-hook
            (lambda ()
              (substitute-key-definition
               'company-complete-common
               'company-yasnippet-or-completion
               company-active-map)))

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
  :commands (pyenv-mode)
  :init
  (setq elpy-rpc-python-command "python3")
  :config
  (defun projectile-pyenv-mode-set ()
    "Set pyenv version matching project name."
    (let ((project (projectile-project-name)))
      (if (member project (pyenv-mode-versions))
          (pyenv-mode-set project)
        (pyenv-mode-unset))))
  (add-hook 'projectile-switch-project-hook 'projectile-pyenv-mode-set)
  (add-hook 'python-mode-hook 'pyenv-mode)

  (use-package pyenv-mode-auto
    :defer t
    :ensure t))


(use-package jedi
  :defer t
  :after 'company
  :ensure t
  :init
  (add-to-list 'company-backends 'company-jedi))


(use-package anaconda-mode
  :commands (anaconda-mode)
  :config
  (use-package company-anaconda
    :ensure t
    :init (add-hook 'python-mode-hook 'anaconda-mode)
    (eval-after-load "company"
      '(add-to-list 'company-backends
                    '(company-anaconda :with company-capf)))))

(use-package rjsx-mode
  :commands rjsx-mode
  :defines (flycheck-check-syntax-automatically)
  :defer t
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.js$" . rjsx-mode))
  :config
  (defun delete-tern-process ()
    "Delete the tern process if it won't go itself'"
    (interactive)
    (delete-process "Tern"))

  (add-hook 'rjsx-mode-hook
            (lambda ()
              (interactive)
              (tide-setup)
              (flycheck-mode +1)
              (set (make-local-variable 'compile-command)
                   (format "npm test"))
              (js2-imenu-extras-setup)
              (flow-minor-enable-automatically)
              (prettier-js-mode)
              (flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append)
              (js2-imenu-extras-mode)
              (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)
              (skewer-mode)
              (js2-refactor-mode)
              (yas-minor-mode)
              (eldoc-mode +1)
              (company-mode +1)
              (setq flycheck-check-syntax-automatically '(save mode-enabled)))))

(use-package js-mode
  :commands (js-mode)
  :defer t
  :hook (js2-minor js2-minor-mode))

(use-package css-mode
  :commands (css-mode)
  :hook (css-mode skewer-css-mode))

(use-package html-mode
  :defer t
  :hook skewer-html-mode)

(use-package web-mode
  :defer t
  :mode ("\\.html\\'"
         "\\.app\\'"
         "\\.cmp\\'"
         "\\.njk\\'"
         "\\.php\\'"
         "\\.phtml\\'"
         "\\.ssp\\'"
         "\\.ejs\\'")
  :hook ((web-mode flyspell-prog-mode) . company-mode)
  :init (setq web-mode-markup-indent-offset 4))

(use-package elxir-mode
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
    :defer t
    :bind (:map alchemist-mode-map
                ([tab] . company-complete))))

(provide 'my-scripting)
