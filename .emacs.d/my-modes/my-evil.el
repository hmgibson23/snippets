(use-package evil
  :ensure t
  :init
  (setq evil-want-integration nil)
  :config
  (setq evil-want-C-i-jump nil)
  (evil-mode 1)
  ;; window splitting
 (defvar my-leader-map (make-sparse-keymap)
  "Keymap for \"leader key\" shortcuts.")

;; binding "," to the keymap

(defvar window-leader-map (make-sparse-keymap)
  "Keymap for \"window key\" shortcuts.")

(define-key evil-normal-state-map "W" window-leader-map)
  (define-key window-leader-map  "q" 'delete-other-windows)
  (define-key window-leader-map  "h" 'split-window-horizontally)
  (define-key window-leader-map  "s" 'split-window-vertically)

  ;; buffers
  (define-key evil-normal-state-map (kbd "gbs") 'ivy-switch-buffer)
  (define-key evil-normal-state-map (kbd "gbo") 'ivy-switch-buffer-other-window)
  ;; operator
;; search-replace

  (define-key evil-normal-state-map (kbd "g%") 'anzu-query-replace)
  (define-key evil-normal-state-map (kbd "gr%") 'anzu-query-replace-regexp)

;; operator
 (define-key evil-operator-state-map "a" evil-outer-text-objects-map)
  (define-key evil-operator-state-map "i" evil-inner-text-objects-map)
  ;; projectile

  (define-key evil-normal-state-map "gpob" 'projectile-switch-to-buffer-other-window)
  (define-key evil-normal-state-map "gpb" 'projectile-switch-to-buffer)

  (define-key evil-normal-state-map "gpf" 'projectile-find-file)
  (define-key evil-normal-state-map "gpF" 'projectile-find-file-other-window)
  (define-key evil-normal-state-map "gpP" 'projectile-switch-project)

 ;; files
  (define-key evil-normal-state-map (kbd "gF") 'find-file-other-window)
  (define-key evil-normal-state-map (kbd "gf") 'find-file)
  (define-key evil-normal-state-map (kbd "T") 'browse-kill-ring)

  )

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))
