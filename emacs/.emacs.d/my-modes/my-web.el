(use-package rjsx-mode
  :commands rjsx-mode
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

(use-package vue-mode
  :commands vue-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.vue$" . vue-mode))
  :config
  (add-hook 'vue-mode-hook
            (lambda ()
              (interactive)
              (message "Loading view mode")
              (setq tide-tsserver-process-environment '("TSS_LOG=-level verbose -file /tmp/tss.log"))
              (tide-setup)

              (flycheck-mode +1)
              (eldoc-mode +1)
              (company-mode +1)
              (setq flycheck-check-syntax-automatically '(save mode-enabled)))))


(use-package inferior-js
  :hook ('comint-output-filter-functions 'js-comint-process-output)
  :config
  (setq inferior-js-program-command "node")
  (setq inferior-js-program-arguments '("--interactive")))

(use-package js-mode
  :hook (js2-minor js2-minor-mode))

(use-package css-mode
  :hook (css-mode skewer-css-mode))

(use-package html-mode
  :hook skewer-html-mode)

(use-package web-mode
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
