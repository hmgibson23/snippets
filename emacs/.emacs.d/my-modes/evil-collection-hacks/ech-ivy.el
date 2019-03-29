;;; ech-ivy.el --- Evil bindings for ivy -*- lexical-binding: t -*-

(require 'general)
(require 'ivy nil t)

(defconst ech-ivy-maps '(ivy-occur-mode-map
                         ivy-occur-grep-mode-map
                         ivy-minibuffer-map))

;;;###autoload
(defun ech-ivy-setup ()
  "Set up `evil' bindings for `ivy-mode'."
  (general-def
    :keymaps 'ivy-mode-map
    (kbd "<escape>") 'minibuffer-keyboard-quit)

  (general-def
    :states 'normal
    :keymaps 'ivy-occur-mode-map
    [mouse-1] 'ivy-occur-click
    (kbd "<return>") 'ivy-occur-press-and-switch
    "n" 'ivy-occur-next-line
    "e" 'ivy-occur-previous-line
    "h" 'evil-backward-char
    "l" 'evil-forward-char
    "g" nil
    "gg" 'evil-goto-first-line
    "gf" 'ivy-occur-press
    "ga" 'ivy-occur-read-action
    "go" 'ivy-occur-dispatch
    "gc" 'ivy-occur-toggle-calling

    ;; refresh
    "gr" 'ivy-occur-revert-buffer

    ;; quit
    "q" 'quit-window)

  (general-def
    :states 'visual
    :keymaps 'ivy-occur-grep-mode-map
    "n" 'evil-next-line
    "e" 'evil-previous-line)

  (general-def
    :states 'normal
    :keymaps 'ivy-occur-grep-mode-map
    "d" 'ivy-occur-delete-candidate
    (kbd "C-x C-q") 'ivy-wgrep-change-to-wgrep-mode
    "i" 'ivy-wgrep-change-to-wgrep-mode
    "gd" 'ivy-occur-delete-candidate
    [mouse-1] 'ivy-occur-click
    (kbd "<return>") 'ivy-occur-press-and-switch
    "n" 'ivy-occur-next-line
    "e" 'ivy-occur-previous-line
    "h" 'evil-backward-char
    "l" 'evil-forward-char
    "g" nil
    "gg" 'evil-goto-first-line
    "gf" 'ivy-occur-press
    "gr" 'ivy-occur-revert-buffer
    "ga" 'ivy-occur-read-action
    "go" 'ivy-occur-dispatch
    "gc" 'ivy-occur-toggle-calling

    "0" 'evil-digit-argument-or-evil-beginning-of-line

    ;; quit
    "q" 'quit-window)


  (general-def
    :states 'normal
    :keymaps 'ivy-minibuffer-map
    (kbd "<escape>") 'abort-recursive-edit
    (kbd "<return>") 'exit-minibuffer
    (kbd "C-m") 'ivy-done
    "n" 'ivy-next-line
    "e" 'ivy-previous-line)

  (general-def
    :states 'insert
    :keymaps 'ivy-minibuffer-map
    [backspace] 'ivy-backward-delete-char
    (kbd "C-r") 'ivy-reverse-i-search
    (kbd "C-n") 'ivy-next-line
    (kbd "C-e") 'ivy-previous-line))

(provide 'ech-ivy)
;;; ech-ivy.el ends here
