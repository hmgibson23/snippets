;; More annoying Mac specific configs
;; because macs are shit unless you
;; love usaeability
(when (equal system-type 'darwin)
  (setq slime-lisp-implementations
        ;; note this is for a MAC - should probably make a
        ;; my-environments.el file for such configurations
        '((clisp ("/usr/local/bin/clisp" "-q -I"))
          (sbcl  ("/usr/local/bin/sbcl") :coding-system utf-8-unix)))

  ;; Mac puts everything in the wrong place
  (setq haskell-program-name "/usr/bin/ghci")
  (setq haskell-process-path-ghci "/usr/bin/ghci")
  (setq ispell-program-name "/usr/local/bin/aspell")
  (setq cider-lein-command "/usr/local/bin/lein")
  (setq geiser-racket-binary "/usr/local/bin/racket")
  (setq slime-csi-path "/usr/local/bin/csi")
  (setq ensime-sbt-command "/usr/local/bin/sbt"))

;; Mac Specific
(when (equal system-type 'darwin)
  (setenv "MANPATH" "/sw/share/man:/usr/share/man")
  (setenv "PATH"
          (concat (getenv "PATH") ":/usr/local/bin")))

(defadvice display-message-or-buffer (before ansi-color activate)
  "Process ANSI color codes in shell output."
  (let ((buf (ad-get-arg 0)))
    (and (bufferp buf)
         (string= (buffer-name buf) "*wr output*")
         (with-current-buffer buf
           (ansi-color-apply-on-region (point-min) (point-max))))))

(let ((my-cabal-path (expand-file-name "~/.cabal/bin")))
  (setenv "PATH" (concat my-cabal-path ":" (getenv "PATH")))
  (add-to-list 'exec-path my-cabal-path))

(exec-path-from-shell-initialize)
