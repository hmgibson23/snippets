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
    :states 'motion
    "gl" 'goto-line

    )

  (general-nmap
    :states 'visual
    "gCr" 'comment-region
    "gCu" 'uncomment-region)

  (general-nmap
    :states 'insert
    (kbd "C-h") 'evil-backward-char
    (kbd "C-l") 'evil-forward-char
    (kbd "C-j") 'evil-next-line
    (kbd "C-k") 'evil-previous-line
    )

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
    :states 'motion
    :prefix-map 'exec-leader-map
    "d" 'docker
    "k" 'kubernetes-overview
    "|" 'shell-command-on-region
    "!" 'shell-command
    "ap" 'async-process
    "as" 'async-shell-command
    "e" 'eshell/here


    ;; buffer operations
    "bk" 'kill-buffer
    "br" 'revert-buffer
    "bo" 'ivy-switch-buffer-other-window
    "by" 'indent-buffer
    "bs" 'ivy-switch-buffer
    "bl" 'list-buffers
    "bi" 'ibuffer
    "be" 'eval-buffer
    "ba" 'mark-whole-buffer

    ;; I do this a lot
    "la" 'insert-line-above
    "lb" 'insert-line-below

    "zxx" 'er/expand-region
    "ci" 'counsel-ibuffer
    "cr" 'counsel-evil-registers
    "cl" 'counsel-locate
    )


  (general-nmap
    :prefix (kbd "SPC")
    :states 'normal
    :prefix-map 'space-leader-map

    ;; counsel

    ;; file operations
    "ff" 'counsel-find-file
    "fr" 'counsel-recentf
    "fF" 'find-file-other-window
    "fl" 'find-file-literally
    "fa" 'find-alternate-file)

  (general-nmap
    :states '(emacs motion )
    :prefix "£"
    :prefix-map 'window-leader-map
    "q" 'delete-other-windows
    "t" 'split-window-horizontally
    "v" 'split-window-vertically
    "j" 'evil-window-down
    "k" 'evil-window-up
    "l" 'evil-window-right
    "h" 'evil-window-left
    "o" 'other-window
    ">" 'next-buffer
    "<" 'previous-buffer
    "bk" 'buf-move-up
    "bj" 'buf-move-down
    "bl" 'buf-move-right
    "bh" 'buf-move-left)

  (evil-set-initial-state 'elfeed-search-mode 'emacs)
  (evil-set-initial-state 'elfeed-show-mode 'emacs)
  (evil-set-initial-state 'elfeed-mode 'emacs)
  (evil-set-initial-state 'hackernews-mode 'emacs)
  (evil-set-initial-state 'comint-mode 'normal)
  )

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
