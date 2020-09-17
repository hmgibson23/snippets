;; -*- lexical-binding: t; -*-;
(use-package general
  :straight t
  :init
  (general-evil-setup)
  :config
  (general-override-mode +1)

  (general-imap
    "C-p" 'fzf
    "C-s" 'swiper-isearch)

  (general-nmap
    "C-s" 'swiper-isearch
    "C-p" 'fzf)

  (general-mmap
    ;; search commands
    "gs%" 'anzu-query-replace
    "gse" 'replace-string
    "gsf" 'search-forward

    "g/"  'dabbrev-expand


    "gz"  'zap-to-char
    "g." 'counsel-yank-pop
    "v" 'evil-visual-char
    "j"  'evil-forward-WORD-end
    "T" 'browse-kill-ring)


  (general-nmap
    :keymaps '(dired-mode-map ibuffer-mode-map)
    "¬" 'hydra-dired/body)

  (general-mmap
    :keymaps 'wgrep-mode-map
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

    "j" 'fasd-find-file

    "dd" 'docker
    "dcc" 'docker-compose
    "dcu" 'docker-compose-up
    "dcd" 'docker-compose-down
    "dcb" 'docker-compose-build
    "dcr" 'docker-compose-restart
    "dk" 'kubernetes-overview

    ";" 'projectile-command-map
    "," 'counsel-M-x
    "mm" 'magit-status

    "mf" 'make-frame
    "sr" 'shell-command-on-region
    "sc" 'shell-command
    "ap" 'async-process
    "as" 'async-shell-command
    "e" 'eshell/here

    "fr" 'rgrep
    "fa" 'ag
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
    :keymaps 'override
    :prefix-map 'spc-leader-map
    "/" 'dabbrev-expand
    "tt" 'xref-find-definitions
    "ts" 'xref-find-apropos

    "y" 'counsel-M-x

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
    :keymaps 'override
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

  ;; colemak related stuff
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

  )

(use-package evil
  :demand
  :straight t
  :init
  (setq evil-want-integration nil)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-i-jump nil)

  :config
  ;; ex commands
  (evil-ex-define-cmd "Q" 'save-buffers-kill-terminal)
  (evil-ex-define-cmd "Ag" 'counsel-ag)
  (evil-ex-define-cmd "Ack" 'counsel-ack)
  (evil-ex-define-cmd "FZF" 'counsel-fzf)
  (evil-ex-define-cmd "Rg" 'counsel-rg)
  (evil-ex-define-cmd "sp" 'evil-split-window-below)
  (evil-ex-define-cmd "ass" 'async-shell-command)

  (with-eval-after-load 'evil
    (require 'ech-compile "$HOME/.emacs.d/my-modes/evil-collection-hacks/ech-compile.el"))

  (evil-mode 1)
  ;; global keybindings
  (define-key key-translation-map [?\C-g] [?\C-c])
  (define-key key-translation-map [?\C-c] [?\C-g])

  ;; evil specific
  (evil-set-initial-state 'elfeed-search-mode 'emacs)
  (evil-set-initial-state 'elfeed-show-mode 'emacs)
  (evil-set-initial-state 'elfeed-mode 'emacs)

  (use-package evil-colemak-basics
    :straight t
    :after (evil general)
    :init
    (setq evil-colemak-basics-rotate-t-f-j nil)
    :config
    (global-evil-colemak-basics-mode 1))

  ;; other modes that are used very much by evil

  (use-package counsel-projectile
    :straight t
    :after (projectile)
    :config
    (counsel-projectile-mode +1))

  (use-package origami
    :straight t
    :defer t
    :config
    (origami-mode +1)))

(use-package key-chord
    :straight t
  :after evil
  :straight t
  :config
  (setq key-chord-two-keys-delay 0.4)
  (key-chord-define evil-insert-state-map "uu" 'evil-normal-state)
  (key-chord-define evil-motion-state-map "uu" 'evil-normal-state)
  (key-chord-mode 1))

(use-package evil-anzu
  :after evil
  :straight t)

(use-package evil-lion
  :after evil
  :straight t
  :commands (evil-lion-mode)
  :config
  (evil-lion-mode))

(use-package evil-goggles
  :after evil
  :straight t
  :commands (evil-goggles-mode)
  :config
  (evil-goggles-use-diff-faces)
  (evil-goggles-mode))

(use-package evil-magit
  :after (evil magit)
  :straight t
  :config
  (setq evil-magit-use-y-for-yank nil))

(use-package evil-expat
  :after evil
  :straight t
  :commands (evil-expat)
  :defer t)

(use-package evil-owl
  :after posframe
  :straight t
  :config
  (setq evil-owl-display-method 'posframe
        evil-owl-extra-posframe-args '(:width 50 :height 20)
        evil-owl-max-string-length 50)
  (evil-owl-mode))

(use-package evil-commentary
  :after evil
  :straight t
  :config
  (with-eval-after-load 'evil
    (evil-commentary-mode)))

(use-package evil-ledger
  :after evil
  :straight t
  :commands (evil-ledger-mode))

(use-package evil-numbers
  :after evil
  :straight t
  :commands (evil-numbers/inc-at-point)
  :defer 10)

(use-package evil-easymotion
  :after evil
  :straight t
  :config
  (evilem-default-keybindings "ge"))

(use-package evil-surround
  :after evil
  :straight t
  :config
  (global-evil-surround-mode +1))


(use-package evil-ex-fasd
  :after evil
  :straight t
  :commands (evil-ex-fasd)
  :config
  (setq evil-ex-fasd-prefix "j "))

(use-package fzf
  :straight t)
(use-package ag
  :straight t)

(use-package evil-terminal-cursor-changer
  :straight t
  :init
  (setq evil-motion-state-cursor 'box)  ; █
  (setq evil-visual-state-cursor 'box)  ; █
  (setq evil-normal-state-cursor 'box)  ; █
  (setq evil-insert-state-cursor 'bar)  ; ⎸
  (setq evil-emacs-state-cursor  'hbar) ; _
  :config
  (evil-terminal-cursor-changer-activate))

(use-package projectile
  :straight t
  :defer 10
  :after (general evil)
  :init
  (setq projectile-enable-caching t)
  (setq projectile-completion-system 'ivy)
  (setq projectile-enable-caching t)
  :config
  (projectile-mode +1))

(provide 'my-evil)
