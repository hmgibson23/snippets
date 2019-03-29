
;;; ech-comint.el --- Bindings for `comint-mode'. -*- lexical-binding: t -*-

(require 'comint)
(require 'general)

(defconst ech-comint-maps '(comint-mode-map))

;;;###autoload
(defun ech-comint-setup ()
  "Set up `evil' bindings for `comint'."
  (general-def
   :states 'normal
   :keymaps 'comint-mode-map
   "gd" 'comint-interrupt-subjob
   (kbd "C-n") #'comint-next-prompt
   (kbd "C-e") #'comint-previous-prompt
   (kbd "gn") #'comint-next-prompt
   (kbd "ge") #'comint-previous-prompt
   (kbd "C-j") #'comint-previous-input
   (kbd "C-k") #'comint-next-input))

(provide 'ech-comint)
;;; ech-comint.el ends here
