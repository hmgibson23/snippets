;;; ech-compile.el --- Evil bindings for `compile' -*- lexical-binding: t -*-

(require 'compile)
(require 'general)

(defconst ech-compile-maps '(compilation-mode-map))

;;;###autoload
(defun ech-compile-setup ()
  "Set up `evil' bindings for `compile'."
  (evil-set-initial-state 'compilation-mode 'normal)

  (general-def
    :states 'normal
    :keymaps 'compilation-mode-map
    "g?" 'describe-mode
    "?" evil-collection-evil-search-backward
    "gg" 'evil-goto-first-line
    "gr" 'recompile
    "0" 'evil-digit-argument-or-evil-beginning-of-line
    [mouse-2] 'compile-goto-error
    [follow-link] 'mouse-face
    (kbd "<return>") 'compile-goto-error

    "go" 'compilation-display-error
    (kbd "S-<return>") 'compilation-display-error

    "gj" 'compilation-next-error
    "gk" 'compilation-previous-error
    (kbd "C-j") 'compilation-next-error
    (kbd "C-k") 'compilation-previous-error
    "[[" 'compilation-previous-file
    "]]" 'compilation-next-file
    "gr" 'recompile))

(provide 'ech-compile)
;;; ech-compile.el ends here
