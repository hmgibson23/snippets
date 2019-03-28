;; -*- lexical-binding: t; -*-;
;; my-functions.el --- List of functions I use
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
  (interactive)
  (let ((trimmed (string-trim str)))
    (string-match "\\([A-Z_]+\\)=\\\"\\([A-Z0-9/+=]+\\)" trimmed)
    (setenv (match-string 1 trimmed) (match-string 2 trimmed))))

(defun read-lines (file)
  "Return a list of lines of a file."
  (with-temp-buffer
    (insert-file-contents file)
    (split-string (buffer-string) "\n" t)))

(defun parse-aws-file ()
  (interactive)
  (defun extract-str (x) (string-match "profile \\([a-z-]+\\)" x) (match-string 1 x))
  ;; \[profile [a-z-]+\]
  (-map 'extract-str (-filter (lambda (x) (string-match "profile \\([a-z-]+\\)" x)) (read-lines "~/.aws/config"))))

(defun assume-role/local (env mfa-code)
  "Allow Emacs to assume a role and be able to execute AWS function on that description"
  (interactive (list (completing-read "Env: " (parse-aws-file))
                     (read-string "MFA Code: ")))
  (assume-role/reset)
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

;; functions for No Starch Press styles
(defun nsp-code-a ()
  (interactive)
  (insert "\n::: {custom-style=\"CodeA\"}\n"))
(defun nsp-code-b ()
  (interactive)
  (insert "\n::: {custom-style=\"CodeB\"}\n"))
(defun nsp-code-c ()
  (interactive)
  (insert "\n::: {custom-style=\"CodeC\"}\n"))
(defun nsp-code-single ()
  (interactive)
  (insert "\n::: {custom-style=\"CodeSingle\"}\n"))
(defun nsp-code-end()
  (interactive)
  (insert "\n:::"))

;;; my-functions.el ends here
