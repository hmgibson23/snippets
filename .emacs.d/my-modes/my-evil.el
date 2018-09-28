(use-package evil
  :ensure t
  :init
  (setq evil-want-integration nil)

  :config
  (setq evil-want-C-i-jump nil)

  (general-evil-setup)
  (evil-mode 1)
  (global-evil-mc-mode  1)

  ;; projectile leader
  (general-nmap
    "gp" 'projectile-command-map

    "gc" 'compile
    ;; search commands
    "gs%" 'anzu-query-replace
    "gsr" 'rgrep
    "gse" 'replace-string
    "gsl" 'lgrep
    "gsz" 'zgrep
    "gsg" 'find-grep
    "gsf" 'search-forward


    "gz"  'zap-to-char
    "g:" 'evil-goto-char
    "g." 'counsel-yank-pop
    "v" 'evil-visual-char
    "T" 'browse-kill-ring
    "gM" 'magit-status
    "gCl" 'comment-line)

  (general-nmap
    :states 'visual
    "gCr" 'comment-region
    "gCu" 'uncomment-region)

  (general-nmap
    :states '(insert emacs)
    "C-h" 'evil-backward-char
    "C-i" 'forward-char
    "C-n" 'evil-next-line
    "C-e" 'evil-previous-line)


  (general-nmap
    :keymaps 'dired-mode-map
    "¬" 'hydra-dired/body)

  (general-nmap
    :keymaps 'ibuffer-mode-map
    "¬" 'hydra-ibuffer-main/body)

  (general-nmap
    :keymaps 'comint-mode-map
    :states 'motion
    "gd" 'comint-interrupt-subjob
    )

  (general-nmap
    :keymaps 'message-mode-map
    :states 'motion
    ";" 'mail-send-and-exit)

  (general-nmap
    :prefix ","
    :states '(motion emacs normal)
    :prefix-map 'exec-leader-map
    "d" 'docker
    "k" 'kubernetes-overview
    "|" 'shell-command-on-region
    "!" 'shell-command
    "ap" 'async-process
    "as" 'async-shell-command
    "e" 'eshell/here

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

    "vv" 'hydra-zoom/body

    "zxx" 'er/expand-region
    "ci" 'counsel-ibuffer
    "cr" 'counsel-evil-registers
    "cl" 'counsel-locate)


  (general-nmap
    :prefix "SPC"
    :states '(motion normal)
    :prefix-map 'spc-leader-map


    "ff" 'counsel-find-file
    "fr" 'counsel-recentf
    "fF" 'find-file-other-window
    "fl" 'find-file-literally
    "fa" 'find-alternate-file
    ;; I do this a lot
    "la" 'insert-line-above
    "lb" 'insert-line-below
)

  (general-nmap
    :states '(emacs motion )
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
  )

(use-package evil-avy
  :after evil
  :ensure t
  :config
  (evil-avy-mode))


(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init)
  )
(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package evil-goggles
  :ensure t
  :config
  (evil-goggles-use-diff-faces)
  (evil-goggles-mode))

(use-package evil-magit
  :defer 1
  :config
  (setq evil-magit-use-y-for-yank nil))

(use-package evil-expat
  :ensure t
  :defer 1)

(use-package evil-ledger)
(use-package evil-numbers)
(use-package evil-dvorak)

(use-package evil-colemak-basics
  :after evil
  :config
  (global-evil-colemak-basics-mode 1))
(use-package key-chord

  :after evil
  :config
  (setq key-chord-two-keys-delay 0.4)
  (key-chord-define evil-motion-state-map "jj" 'evil-normal-state)
  (key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
  (key-chord-define evil-insert-state-map "[[" 'save-buffer)
  (key-chord-define evil-motion-state-map "[[" 'save-buffer)
  (key-chord-mode 1))
