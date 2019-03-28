;; -*- lexical-binding: t; -*-;
(use-package evil
  :ensure t
  :after (general)
  :init
  (setq evil-want-integration nil)
  (setq evil-want-keybinding nil)
  (general-evil-setup)
  (evil-mode 1)

  :config
  (setq evil-want-C-i-jump nil)
  (with-eval-after-load 'evil
    (require 'evil-anzu))

  ;; global keybindings
  (keyboard-translate ?\C-g ?\C-c)
  (keyboard-translate ?\C-c ?\C-g)

  ;; evil specific
  (general-mmap
    ;; search commands
    "gs%" 'anzu-query-replace
    "gse" 'replace-string
    "gsf" 'search-forward

    "g/"  'dabbrev-expand

    "gz"  'zap-to-char
    "g:" 'evil-goto-char
    "g." 'counsel-yank-pop
    "v" 'evil-visual-char
    "T" 'browse-kill-ring)


  (general-nmap
    :keymaps '(dired-mode-map ibuffer-mode-map)
    "¬" 'hydra-dired/body)


  (general-mmap
    :keymaps 'comint-mode-map
    "gd" 'comint-interrupt-subjob)

  (general-nmap
    :keymaps 'compilation-mode-map
    "gd" 'comint-interrupt-subjob
    "gc" 'compile
    "gg" 'recompile)

  (general-mmap
    :keymaps 'dired-mode-map
    "gww" 'wgrep-change-to-wgrep-mode
    "gwr" 'wgrep-finish-edit
    "gws" 'wgrep-save-all-buffers)

  (general-mmap
    :keymaps 'go-mode-map
    "gdd" 'dlv)

  (general-mmap
    :keymaps 'message-mode-map
    ";" 'mail-send-and-exit)

  (general-create-definer exec-leader
    :prefix ",")

  (exec-leader
    :states 'motion
    :keymap 'flycheck-mode-map
    :prefix-map 'exec-leader-map
    "fn" 'flycheck-next-error
    "fl" 'flycheck-list-errors)


  (exec-leader
    :states 'motion
    :keymap 'go-mode-map
    :prefix-map 'exec-leader-map
    "gr" 'go-rename
    "gd" 'godoc-at-point
    "gj" 'godef-jump)

  (exec-leader
    :states 'motion
    :keymap 'plantuml-mode-map
    :prefix-map 'exec-leader-map
    "cp" 'plantuml-preview
    "cb" 'plantuml-preview-current-block)

  (exec-leader
    :states 'motion
    :keymap 'markdown-mode-map
    :prefix-map 'exec-leader-map
    "." 'hydra-markdown/body
    "na" 'nsp-code-a
    "nb" 'nsp-code-b
    "nc" 'nsp-code-c
    "ns" 'nsp-code-single
    "nl" 'nsp-code-end)

  (exec-leader
    :states 'motion
    :prefix-map 'exec-leader-map

    "dd" 'docker
    "dcc" 'docker-compose
    "dcu" 'docker-compose-up
    "dcd" 'docker-compose-down
    "dcb" 'docker-compose-build
    "dcr" 'docker-compose-restart
    "dk" 'kubernetes-overview

    ";" 'projectile-command-map
    "mm" 'magit-status

    "mf" 'make-frame
    "sr" 'shell-command-on-region
    "sc" 'shell-command
    "ap" 'async-process
    "as" 'async-shell-command
    "e" 'eshell/here

    "fr" 'rgrep
    "fl" 'lgrep
    "fz" 'zgrep
    "fg" 'find-grep

    "bk" 'ido-kill-buffer
    "br" 'revert-buffer
    "bo" 'ivy-switch-buffer-other-window
    "bs" 'ivy-switch-buffer
    "bl" 'list-buffers
    "bi" 'ibuffer
    "be" 'eval-buffer
    "ba" 'mark-whole-buffer
    "bg" 'switch-to-gnus

    "re" 'exit-recursive-edit
    "ra" 'abort-recursive-edit

    "vv" 'hydra-zoom/body


    "mu" 'evil-mc-undo-all-cursors
    "m." 'evil-mc-make-cursor-move-next-line


    "gc" 'compile

    "zxx" 'er/expand-region
    "ci" 'counsel-ibuffer
    "ca" 'counsel-ag
    "cr" 'counsel-rg
    "coa" 'counsel-org-agenda-headlines
    "coc" 'counsel-org-capture
    "cm" 'counsel-semantic-or-imenu
    "ce" 'counsel-evil-registers
    "cl" 'counsel-locate)

  (defvar extra-key-modes '(magit-mode-map dired-mode-map ibuffer-mode-hook grep-mode compilation-mode info-mode))

  (exec-leader
    :states '(emacs motion)
    :keymaps '(magit-mode-map dired-mode-map ibuffer-mode-hook grep-mode compilation-mode info-mode)
    :prefix-map 'exec-leader-map)

  (general-create-definer spc-leader
    :prefix "SPC")

  (spc-leader
    :states 'motion
    :prefix-map 'spc-leader-map
    "/" 'dabbrev-expand
    "tt" 'xref-find-definitions
    "ts" 'xref-find-apropos

    "son" 'scroll-other-window
    "sop" 'scroll-other-window-down
    "ff" 'counsel-find-file
    "fr" 'counsel-recentf
    "fF" 'find-file-other-window
    "fl" 'find-file-literally
    "fa" 'find-alternate-file
    ;; I do this a lot
    "la" 'insert-line-above
    "lb" 'insert-line-below)

  (spc-leader
    :states 'emacs
    :prefix-map 'spc-leader-map
    :keymaps '(magit-mode-map dired-mode-map compilation-mode))

  (spc-leader
    :keymap 'docker-image-mode-map
    :states 'motion
    "m" 'tablist-mark-forward
    "u" 'tablist-unmark-forward
    "l" 'docker-image-ls-popup
    "D" 'docker-image-rm-popup
    "F" 'docker-image-pull-popup
    "P" 'docker-image-push-popup
    "R" 'docker-image-run-popup)

  (spc-leader
    :keymap 'docker-container-mode-map
    :states 'motion

    "m" 'tablist-mark-forward
    "u" 'tablist-unmark-forward
    "?" 'docker-container-help-popup
    "C" 'docker-container-cp-popup
    "D" 'docker-container-rm-popup
    "I" 'docker-container-inspect-popup
    "K" 'docker-container-kill-popup
    "L" 'docker-container-logs-popup
    "O" 'docker-container-stop-popup
    "P" 'docker-container-pause-popup
    "R" 'docker-container-restart-popup
    "S" 'docker-container-start-popup
    "a" 'docker-container-attach-popup
    "b" 'docker-container-shell-popup
    "d" 'docker-container-diff-popup)

  (general-def
    :keymaps '(cider-repl-mode-map)
    :states '(emacs motion insert)
    "M-e" 'cider-repl-previous-input
    "M-n" 'cider-repl-next-input)

  (exec-leader
    :states 'motion
    :keymaps '(cider-mode-map)
    :prefix-map 'exec-leader-map
    "be" 'cider-eval-buffer)

  (general-def
    :states '(emacs motion)
    :prefix "C-w"
    :prefix-map 'window-leader-map
    "o" 'delete-other-windows
    "t" 'split-window-vertically
    "v" 'split-window-horizontally
    "n" 'evil-window-down
    "e" 'evil-window-up
    "i" 'evil-window-right
    "h" 'evil-window-left
    ";" 'other-frame
    ">" 'next-buffer
    "<" 'previous-buffer
    "bn" 'buf-move-up
    "be" 'buf-move-down
    "bi" 'buf-move-right
    "bh" 'buf-move-left)

  (evil-set-initial-state 'elfeed-search-mode 'emacs)
  (evil-set-initial-state 'elfeed-show-mode 'emacs)
  (evil-set-initial-state 'elfeed-mode 'emacs)

  (use-package evil-goggles
    :defer t
    :config
    (evil-goggles-use-diff-faces)
    (evil-goggles-mode))

  ;; (use-package evil-magit
  ;;   :defer t
  ;;   :config
  ;;   (setq evil-magit-use-y-for-yank nil))

  (use-package evil-expat
    :defer t)

  (use-package evil-collection
    :after evil
    :defer t
    :config
    (evil-collection-init))

  (use-package evil-commentary
    :after evil
    :defer t
    :config
    (evil-commentary-mode))

  (use-package evil-ledger
    :defer 1)

  (use-package evil-numbers
    :defer 1)

  (use-package evil-easymotion
    :defer 1
    :config
    (evilem-default-keybindings ",,"))

  (use-package evil-surround
    :defer 1
    :config
    (global-evil-surround-mode 1))

  (use-package evil-colemak-basics
    :after (evil general)
    :config
    (global-evil-colemak-basics-mode 1)

    (general-def
      :states '(insert emacs motion)
      "TAB" 'indent-for-tab-command
      "C-h" 'evil-backward-char
      "C-n" 'evil-next-line
      "C-o" 'forward-char
      "C-e" 'evil-previous-line)

    (general-nmap
      :keymaps '(comint-mode-map)
      "]" 'comint-next-input
      "[" 'comint-previous-input)

    (general-imap
      :keymaps '(comint-mode-map)
      "C-n" 'comint-next-input
      "C-e" 'comint-previous-input)

    (general-imap
      :keymaps '(term-mode-map)
      "C-n" 'term-send-up
      "C-e" 'term-send-down)

    (general-nmap
      :keymaps '(term-mode-map)
      "C-n" 'term-send-up
      "C-e" 'term-send-down)

    (general-nmap
      "j" 'evil-forward-WORD-end)

    (general-def
      :keymaps 'ivy-minibuffer-map
      "C-n" 'ivy-next-line
      "C-e" 'ivy-previous-line))

  ;; other modes that are used very much by evil
  (use-package projectile
    :defer t
    :after (evil)
    :config
    (setq projectile-enable-caching t)
    (setq projectile-completion-system 'ivy)
    (setq projectile-enable-caching t)
    (projectile-mode +1)
    )
  (use-package counsel-projectile :defer t
    :config
    (counsel-projectile-mode +1)
    )

  )


(use-package key-chord
  :after evil
  :config
  (setq key-chord-two-keys-delay 0.4)
  (key-chord-define evil-insert-state-map "uu" 'evil-normal-state)
  (key-chord-define evil-motion-state-map "uu" 'evil-normal-state)
  (key-chord-mode 1))


(provide 'my-evil)
