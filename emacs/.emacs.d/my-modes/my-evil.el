;; -*- lexical-binding: t; -*-;
(use-package evil
  :ensure t
  :init
  (setq evil-want-integration nil)
  (setq evil-want-keybinding nil)
  :config
  (setq evil-want-C-i-jump nil)
  (fset 'evil-visual-update-x-selection 'ignore)
  (general-evil-setup)
  (evil-mode 1)
  (global-evil-mc-mode  1)

  ;; projectile leader
  (general-nmap

    "gc" 'compile
    ;; search commands
    "gs%" 'anzu-query-replace
    "gse" 'replace-string
    "gsf" 'search-forward


    "gz"  'zap-to-char
    "g:" 'evil-goto-char
    "g." 'counsel-yank-pop
    "v" 'evil-visual-char
    "T" 'browse-kill-ring
    "gCl" 'comment-line)

  (general-nmap
    :states 'visual
    "gCr" 'comment-region
    "gCu" 'uncomment-region)

  (general-nmap
    :keymaps 'dired-mode-map
    "¬" 'hydra-dired/body)

  (general-nmap
    :keymaps 'ibuffer-mode-map
    "¬" 'hydra-ibuffer-main/body)

  ;; (general-nmap
  ;;   :keymaps 'magit-mode-map
  ;;   "f" 'magit-fetch-popup
  ;;   )

  (general-nmap
    :keymaps 'comint-mode-map
    :states 'motion
    "gd" 'comint-interrupt-subjob
    )

  (general-nmap
    :keymaps 'message-mode-map
    :states 'motion
    ";" 'mail-send-and-exit)

  (general-create-definer exec-leader
    :prefix ",")

  (exec-leader
    :states 'motion
    :keymaps 'flycheck-mode-map
    :prefix-map 'exec-leader-map
    "fn" 'flycheck-next-error
    "fl" 'flycheck-list-errors)

  (exec-leader
    :states 'motion
    :prefix-map 'exec-leader-map
    "d" 'docker
    "k" 'kubernetes-overview
    "p" 'projectile-command-map
    "mm" 'magit-status
    "sr" 'shell-command-on-region
    "sc" 'shell-command
    "ap" 'async-process
    "as" 'async-shell-command
    "e" 'eshell/here

    "fr" 'rgrep
    "fl" 'lgrep
    "fz" 'zgrep
    "fg" 'find-grep

    "j"  'evil-avy-goto-char
    "bk" 'kill-buffer
    "br" 'revert-buffer
    "bo" 'ivy-switch-buffer-other-window
    "by" 'indent-buffer
    "bs" 'ivy-switch-buffer
    "bl" 'list-buffers
    "bi" 'ibuffer
    "be" 'eval-buffer
    "ba" 'mark-whole-buffer
    "bg" 'switch-to-gnus

    "re" 'exit-recursive-edit
    "ra" 'abort-recursive-edit

    "vv" 'hydra-zoom/body

    "zxx" 'er/expand-region
    "ci" 'counsel-ibuffer
    "cr" 'counsel-evil-registers
    "cl" 'counsel-locate)

  (exec-leader
    :states 'emacs
    :keymaps '(magit-mode-map dired-mode-map ibuffer-mode-hook)
    :prefix-map 'exec-leader-map)

  (general-create-definer spc-leader
    :prefix "SPC")

  (spc-leader
    :states 'motion
    :prefix-map 'spc-leader-map

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
    :keymaps '(magit-mode-map dired-mode-map))

  (general-nmap
    :states '(emacs motion)
    :prefix "£"
    :prefix-map 'window-leader-map
    "q" 'delete-other-windows
    "t" 'split-window-horizontally
    "v" 'split-window-vertically
    "n" 'evil-window-down
    "e" 'evil-window-up
    "i" 'evil-window-right
    "h" 'evil-window-left
    "o" 'other-window
    ">" 'next-buffer
    "<" 'previous-buffer
    "bn" 'buf-move-up
    "be" 'buf-move-down
    "bi" 'buf-move-right
    "bh" 'buf-move-left)


  (evil-set-initial-state 'elfeed-search-mode 'emacs)
  (evil-set-initial-state 'elfeed-show-mode 'emacs)
  (evil-set-initial-state 'elfeed-mode 'emacs)
  (evil-set-initial-state 'comint-mode 'normal)
  (evil-set-initial-state 'dired-mode 'emacs)
  (evil-set-initial-state 'sql 'emacs))

(use-package evil-avy
  :after evil
  :ensure t
  :defer 1
  :config
  (evil-avy-mode))


(use-package evil-surround
  :ensure t
  :defer 1
  :config
  (global-evil-surround-mode 1))

(use-package evil-goggles
  :ensure t
  :defer 1
  :config
  (evil-goggles-use-diff-faces)
  (evil-goggles-mode))

;; (use-package evil-magit
;;   :defer 1
;;   :config
;;   (setq evil-magit-use-y-for-yank nil))

(use-package evil-expat
  :defer 1
  :ensure t
  :defer 1)

(use-package evil-ledger
  :defer 1)
(use-package evil-numbers
  :defer 1)

(use-package evil-colemak-basics
  :after evil
  :defer 1
  :config
  (global-evil-colemak-basics-mode 1)

  (general-nmap
    :states '(insert emacs motion)
    "TAB" 'indent-for-tab-command
    "C-h" 'evil-backward-char
    "C-n" 'evil-next-line
    "C-o" 'forward-char
    "C-e" 'evil-previous-line)

  (general-nmap
    :states 'normal
    :keymaps '(comint-mode-map)
    "]" 'comint-next-input
    "[" 'comint-previous-input)

  (general-nmap
    :keymaps 'ivy-minibuffer-map
    "C-n" 'ivy-next-line
    "C-e" 'ivy-previous-line))

(use-package key-chord
  :defer 2
  :after evil
  :config
  (setq key-chord-two-keys-delay 0.4)
  (key-chord-define evil-motion-state-map "jj" 'evil-normal-state)
  (key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
  (key-chord-define evil-insert-state-map "[[" 'save-buffer)
  (key-chord-define evil-motion-state-map "[[" 'save-buffer)
  (key-chord-mode 1))
