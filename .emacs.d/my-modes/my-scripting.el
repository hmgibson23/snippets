;; everything needed for srcipting languages
(use-package ruby-mode
  :mode "\\.rb\\'"
  :interpreter "ruby"
  ;;  :hook ((ruby-mode inf-ruby-minor-mode ruby-end-mode inf-ruby-minor-mode inf-ruby-switch-setup yard-mode) . rspec-mode)

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
    '(add-to-list 'company-backends 'company-inf-ruby))


  ;; (eval-after-load 'rspec-mode
  ;;   '(rspec-install-snippets))
  )


(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  ;;  :hook ((python-mode anaconda-mode) . anaconda-eldoc-mode)
  :init
  ;; (setq python-shell-interpreter "/usr/local/bin/ipython"
  ;;       python-shell-interpreter-args "--simple-prompt -i"
  ;;       python-shell-prompt-regexp "In \\[[0-9]+\\]: "
  ;;       python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
  ;;       python-shell-completion-setup-code
  ;;       "from IPython.core.completerlib import module_completion"
  ;;       python-shell-completion-module-string-code "';'.join(module_completion('''%s'''))\n"
  ;;       python-shell-completion-string-code "';'.join(get_ipython().Completer.all_completions('''%s'''))\n"
  ;;       python-shell-output-filter 'ansi-color-for-comint-mode-filter)
  :config
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
    (highlight-lines-matching-regexp "pdb.set_trace()"))


  )
(use-package pyenv-mode
  :ensure t
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
  (add-hook 'python-mode-hook 'pyenv-mode))

(use-package pyenv-mode-auto
  :ensure t)

(use-package jedi
  :ensure t
  :init
  (add-to-list 'company-backends 'company-jedi))

(use-package company-jedi
  :ensure t
  :init
  (add-hook 'python-mode-hook (lambda () (add-to-list 'company-backends 'company-jedi)))
  (setq company-jedi-python-bin "python"))


(use-package anaconda-mode
  :ensure t
  :config
  (use-package company-anaconda
    :ensure t
    :init (add-hook 'python-mode-hook 'anaconda-mode)
    (eval-after-load "company"
      '(add-to-list 'company-backends
                    '(company-anaconda :with company-capf)))))
