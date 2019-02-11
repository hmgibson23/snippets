;; -*- lexical-binding: t; -*-;
;; Shell organisation stuff
(require 'eshell)
;; Eshell
;; Prettify shell prompt
(setq eshell-prompt-function
      (lambda nil (concat
              (abbreviate-file-name
               (first (last (split-string (eshell/pwd) "/"))))
              (if (= (user-uid)  0) " # " " $ "))))


;; sort out process colours
(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'magit-commit-hook 'colorize-magit-buffer)

;; Terminal colouring
(add-hook 'shell-mode-hook
          (lambda () (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter nil t)))


(add-hook 'eshell-before-prompt-hook
          (lambda ()
            (setq xterm-color-preserve-properties t)))

(add-to-list 'eshell-preoutput-filter-functions 'ansi-color-filter-apply)
(setq eshell-output-filter-functions (remove 'eshell-handle-ansi-color eshell-output-filter-functions))

;; Terminals do not use snippets
(add-hook 'term-mode-hook
          (lambda()
            (message "Turning off company mode")
            (setq yas-dont-activate t)
            (company-mode -1)))

;; make the colour right
(eval-after-load 'compile
  '(add-hook 'compilation-filter-hook
        (lambda () (ansi-color-process-output nil))))

(setq compilation-environment '("TERM=xterm-256color"))

(add-hook 'compilation-start-hook
          (lambda (proc)
            ;; We need to differentiate between compilation-mode buffers
            ;; and running as part of comint (which at this point we assume
            ;; has been configured separately for xterm-color)
            (when (eq (process-filter proc) 'compilation-filter)
              ;; This is a process associated with a compilation-mode buffer.
              ;; We may call `xterm-color-filter' before its own filter function.
              (set-process-filter
               proc
               (lambda (proc string)
                 (funcall 'compilation-filter proc
                          (xterm-color-filter string)))))))


(defun setup-eshell ()
  (define-key eshell-mode-map (kbd "<tab>") 'completion-at-point))

(add-hook 'eshell-mode-hook 'setup-eshell)

(provide 'my-shell)
