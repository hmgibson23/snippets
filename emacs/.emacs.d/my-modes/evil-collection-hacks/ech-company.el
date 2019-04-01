
;;; ech-company.el --- Bindings for `company-mode'. -*- lexical-binding: t -*-

(require 'company nil t)
(require 'general)

(declare-function company-tng-configure-default "company-tng")

(defgroup ech-company nil
  "Evil bindings for `company-mode'."
  :group 'evil-collection)

(defvar company-active-map)
(defvar company-search-map)

(defconst ech-company-maps '(company-active-map company-search-map))

;;;###autoload

(defun ech-company-setup ()
  "Set up `evil' bindings for `company'."
  (general-def
:keymaps 'company-active-map
    (kbd "C-n") 'company-select-next-or-abort
    (kbd "C-e") 'company-select-previous-or-abort
    (kbd "M-n") 'company-select-next
    (kbd "M-e") 'company-select-previous)

  (general-def
:keymaps 'company-search-map
    (kbd "C-n") 'company-select-next-or-abort
    (kbd "C-e") 'company-select-previous-or-abort
    (kbd "M-n") 'company-select-next
    (kbd "M-e") 'company-select-previous
    (kbd "<escape>") 'company-search-abort)

  ;; Sets up YCMD like behavior.
  (company-tng-configure-default))

(provide 'ech-company)
;;; ech-company.el ends here
