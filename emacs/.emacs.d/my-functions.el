;;; my-functions.el --- List of functions I use
;; These are all my functions that I mostly stole
;; from other people but they don't mind because
;; they treat everyone with love like me


;;; Commentary:
;;

;;; Code:

(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (indent-buffer)
  (untabify-buffer)
  (delete-trailing-whitespace))
(defun cleanup-region (beg end)
  "Remove tmux artifacts from region."
  (interactive "r")
  (dolist (re '("\\\\│\·*\n" "\W*│\·*"))
    (replace-regexp re "" nil beg end)))

(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))


;; useful sudo-edit function to avoid excessive typing
(defun sudo-edit (&optional arg)
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun ido-imenu ()
  "Update the imenu index and then use ido to select a symbol to navigate to.
Symbols matching the text at point are put first in the completion list."
  (interactive)
  (imenu--make-index-alist)
  (let ((name-and-pos '())
        (symbol-names '()))
    (flet ((addsymbols (symbol-list)
                       (when (listp symbol-list)
                         (dolist (symbol symbol-list)
                           (let ((name nil) (position nil))
                             (cond
                              ((and (listp symbol) (imenu--subalist-p symbol))
                               (addsymbols symbol))

                              ((listp symbol)
                               (setq name (car symbol))
                               (setq position (cdr symbol)))

                              ((stringp symbol)
                               (setq name symbol)
                               (setq position (get-text-property 1 'org-imenu-marker symbol))))

                             (unless (or (null position) (null name))
                               (add-to-list 'symbol-names name)
                               (add-to-list 'name-and-pos (cons name position))))))))
      (addsymbols imenu--index-alist))
    ;; If there are matching symbols at point, put them at the beginning of `symbol-names'.
    (let ((symbol-at-point (thing-at-point 'symbol)))
      (when symbol-at-point
        (let* ((regexp (concat (regexp-quote symbol-at-point) "$"))
               (matching-symbols (delq nil (mapcar (lambda (symbol)
                                                     (if (string-match regexp symbol) symbol))
                                                   symbol-names))))
          (when matching-symbols
            (sort matching-symbols (lambda (a b) (> (length a) (length b))))
            (mapc (lambda (symbol) (setq symbol-names (cons symbol (delete symbol symbol-names))))
                  matching-symbols)))))
    (let* ((selected-symbol (ido-completing-read "Symbol? " symbol-names))
           (position (cdr (assoc selected-symbol name-and-pos))))
      (goto-char position))))

;; full screen
(defun fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen
                       (if (frame-parameter nil 'fullscreen) nil 'fullboth)))

(defun save-and-reload ()
  "Save and reload the browse"
  (save-buffer)
  (shell-command "chrome-reload")
  (shell-command "stumpish 'gselect 4'"))

(defun copy-file-location ()
  "Put the current buffer's file location in the kill ring"
  (interactive)
  (kill-new (message (buffer-file-name))))


(defun replace-last-sexp ()
  "Evaluate Lisp in place and replace, killing the previous sexp"
  (interactive)
  (let ((value (eval (preceding-sexp))))
    (kill-sexp -1)
    (insert (format "%S" value))))


;; Again stolen from someone else I'm such a thief
(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match that used by the user's shell.

This is particularly useful under Mac OSX, where GUI apps are not started from a shell."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))


(defun switch-buffer-scratch ()
  "Switch to the scratch buffer. If the buffer doesn't exist,
create it and write the initial message into it."
  (interactive)
  (let* ((scratch-buffer-name "*scratch*")
         (scratch-buffer (get-buffer scratch-buffer-name)))
    (unless scratch-buffer
      (setq scratch-buffer (get-buffer-create scratch-buffer-name))
      (with-current-buffer scratch-buffer
        (lisp-interaction-mode)
        (insert initial-scratch-message)))
    (switch-to-buffer scratch-buffer)))


(defun
    flymake-haskell-init ()
  "When flymake triggers, generates a tempfile containing the
  contents of the current buffer, runs `hslint` on it, and
  deletes file. Put this file path (and run `chmod a+x hslint`)
  to enable hslint: https://gist.github.com/1241073"
  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
         (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
    (list "hslint" (list local-file))))

(defun flymake-haskell-enable ()
  "Enables flymake-mode for haskell, and sets <C-c d> as command
  to show current error."
  (when (and buffer-file-name
             (file-writable-p
              (file-name-directory buffer-file-name))
             (file-writable-p buffer-file-name))
    (local-set-key (kbd "C-c d") 'flymake-display-err-menu-for-current-line)
    (flymake-mode t)))

(defun my-haskell-mode-hook ()
  "hs-lint binding, plus autocompletion and paredit."
  (local-set-key "\C-cl" 'hs-lint)
  (dolist (x '(haskell literate-haskell))
    (add-hook
     (intern (concat (symbol-name x)
                     "-mode-hook"))
     'turn-on-paredit)))


(defun get-home-dir ()
  "Print the full path to the current user's home directory")


(defun async-process (command args)
  "Run an async process in an attached buffer"
  (interactive "sCommand: \nsArguments: ")
  (start-process-shell-command  command (concat "*" command " output*")
                                command args)
  (with-current-buffer
      (process-buffer (get-process command))
    (funcall 'shell-mode))
  (set-process-filter (get-process command) 'filter-process-output))



(defun wr-watch (file command)
  "Run the wr command in the background and print its output"
  (interactive (list (read-file-name "File to watch: ")
                     (read-string "Command to execute: ")))
  (start-process-shell-command "wr" "*wr output*" "wr" (concat "\"" command "\"") file)
  (with-current-buffer
      (process-buffer (get-process "wr"))
    (funcall 'shell-mode))
  (set-process-filter (get-process "wr") 'filter-process-output))

(defun parse-num (string)
  "Get first number from a string - useful for exit codes"
  (string-match "[[:digit:]]" string)
  (string-to-number (match-string 0 string)))

(defun filter-process-output (process output)
  (with-current-buffer (process-buffer process)
    (insert (ansi-color-apply output))))

(defun ansi-color-apply-on-region-int (beg end)
  "interactive version of func"
  (interactive "r")
  (ansi-color-apply-on-region beg end))

(defun colorize-magit-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))



(defun absolute-dirname (path)
  "Return the directory name portion of a path.

If PATH is local, return it unaltered.
If PATH is remote, return the remote diretory portion of the path."
  (if (tramp-tramp-file-p path)
      (elt (tramp-dissect-file-name path) 3)
    path))

(defun get-string-from-file (filePath)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(defun read-lines (filePath)
  "Return a list of lines of a file at filePath."
  (with-temp-buffer
    (insert-file-contents filePath)
    (split-string (buffer-string) "\n" t)))

(defun my-load-all-in-directory (dir)
  "`load' all elisp libraries in directory DIR which are not already loaded."
  (interactive "D")
  (let ((libraries-loaded (mapcar #'file-name-sans-extension
                                  (delq nil (mapcar #'car load-history)))))
    (dolist (file (directory-files dir t ".+\\.elc?$"))
      (let ((library (file-name-sans-extension file)))
        (unless (member library libraries-loaded)
          (load library nil t)
          (push library libraries-loaded))))))

(defun buffer-mode (buffer-or-string)
  "Returns the major mode associated with a buffer."
  (with-current-buffer buffer-or-string
    major-mode))

(defun add-exec-path (path)
  "Add a path to the execpath"
  (interactive "D")
  (add-to-list 'exec-path path))

(defun tab-indent-or-complete ()
  (interactive)
  (cond
   ((minibufferp)
    (minibuffer-complete))
   (t
    (indent-for-tab-command)
    (if (or (not yas/minor-mode)
            (null (do-yas-expand)))
        (if (check-expansion)
            (progn
              (company-manual-begin)
              (if (null company-candidates)
                  (progn
                    (company-abort)
                    (indent-for-tab-command)))))))))

(defun convert-odt-to-docx (file)
  (interactive (list (read-file-name "File to convert")))
  (shell-command
   (concat "libreoffice --headless --convert-to docx " file)))


(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

(defun run-async-command-in-dir (directory command)
  "Runs an async shell command in a directory of your choice."
  (interactive (list (read-directory-name "Root dir: ")
                     (read-string "Command: ")))
  (let ((default-directory directory))
    (start-process-shell-command command "*Async command*" command))
  (with-current-buffer
      (process-buffer (get-process command))
    (erase-buffer)
    (funcall 'shell-mode))
  (set-process-filter (get-process command) 'filter-process-output))


(defun set-env-from-kv-str (str)
  (let ((trimmed (string-trim str)))
    (string-match "\\([A-Z_]+\\)=\\\"\\([A-Z0-9/+=]+\\)" trimmed)
    (setenv (match-string 1 trimmed) (match-string 2 trimmed))))

(defun read-lines (file)
  "Return a list of lines of a file."
  (with-temp-buffer
    (insert-file-contents file)
    (split-string (buffer-string) "\n" t)))

(defun parse-aws-file ()
  (defun extract-str (x) (string-match "profile \\([a-z-]+\\)" x) (match-string 1 x))
  (interactive)
  ;; \[profile [a-z-]+\]
  (-map 'extract-str (-filter (lambda (x) (string-match "profile \\([a-z-]+\\)" x)) (read-lines "~/.aws/config"))))

(defun assume-role/local (env mfa-code)
  "Allow Emacs to assume a role and be able to execute AWS function on that description"
  (assume-role/reset)
  (interactive (list (completing-read "Env: " (parse-aws-file))
                     (read-string "MFA Code: ")))
  (message env)
  (message mfa-code)
  (message (format "echo %s | assume-role %s" mfa-code env))
  (let ((out (shell-command-to-string (format "echo %s | assume-role %s" mfa-code env))))
    (message out)
    (-map 'set-env-from-kv-str
          (-filter (lambda (x) (cl-search "export" x)) (split-string (car (cdr (split-string out ":"))) "\n")))))

(defun assume-role/reset ()
  (interactive)
  (-map (lambda (x) (setenv x ""))
        '("ASSUMED_ROLE"
          "AWS_SECURITY_TOKEN"
          "AWS_SESSION_TOKEN"
          "AWS_SECRET_ACCESS_KEY"
          "AWS_ACCESS_KEY_ID"
          "ASSUMED_ROLE")))

(defun eshell/here ()
  (interactive)
  (with-current-buffer
      (eshell "new")
    (rename-buffer (concat "*eshell-" (car (last (delete "" (split-string default-directory "/")))) "*"))))

(defun simba-dev ()
  (setenv KUBECONFIG "~/.kube/config-dev")
  (setenv SIMBA_ENV  "dev"))

(defun insert-line-below ()
  "Insert an empty line below the current line."
  (interactive)
  (save-excursion
    (end-of-line)
    (open-line 1)))

(defun insert-line-above ()
  "Insert an empty line above the current line."
  (interactive)
  (save-excursion
    (end-of-line 0)
    (open-line 1)))
(provide 'my-functions)

 (defun insert-current-date () (interactive)
    (insert (shell-command-to-string "echo -n $(date +%Y-%m-%d)")))

;;; my-functions.el ends here
