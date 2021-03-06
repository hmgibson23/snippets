
;;; ech-dired.el --- Evil bindings for Dired -*- lexical-binding: t -*-

(require 'dired)
(require 'general)

(defconst evil-collection-dired-maps '(dired-mode-map))

;;;###autoload
(defun ech-dired-setup ()
  "Set up `evil' bindings for `dired'."
  (general-def
    :states '(normal)
    :keymaps '(dired-mode-map)
    "q" 'quit-window
    "n" 'dired-next-line
    "e" 'dired-previous-line
    [mouse-2] 'dired-mouse-find-file-other-window
    [follow-link] 'mouse-face
    ;; Commands to mark or flag certain categories of files
    "#" 'dired-flag-auto-save-files
    "." 'dired-clean-directory
    "~" 'dired-flag-backup-files
    ;; Upper case keys (except !) for operating on the marked files
    "A" 'dired-do-find-regexp
    "C" 'dired-do-copy
    "B" 'dired-do-byte-compile
    "D" 'dired-do-delete
    "gG" 'dired-do-chgrp ;; FIXME: This can probably live on a better binding.
    "H" 'dired-do-hardlink
    "L" 'dired-do-load
    "M" 'dired-do-chmod
    "O" 'dired-do-chown
    "P" 'dired-do-print
    "Q" 'dired-do-find-regexp-and-replace
    "R" 'dired-do-rename
    "S" 'dired-do-symlink
    "T" 'dired-do-touch
    "X" 'dired-do-shell-command
    "Z" 'dired-do-compress
    "c" 'dired-do-compress-to
    "!" 'dired-do-shell-command
    "&" 'dired-do-async-shell-command
    ;; Comparison commands
    "=" 'dired-diff
    ;; Tree Dired commands
    (kbd "M-C-?") 'dired-unmark-all-files
    (kbd "M-C-d") 'dired-tree-down
    (kbd "M-C-u") 'dired-tree-up
    (kbd "M-C-n") 'dired-next-subdir
    (kbd "M-C-p") 'dired-prev-subdir
    ;; move to marked files
    (kbd "M-{") 'dired-prev-marked-file
    (kbd "M-}") 'dired-next-marked-file
    ;; Make all regexp commands share a `%' prefix:
    ;; We used to get to the submap via a symbol dired-regexp-prefix,
    ;; but that seems to serve little purpose, and copy-keymap
    ;; does a better job without it.
    "%" nil
    "%u" 'dired-upcase
    "%l" 'dired-downcase
    "%d" 'dired-flag-files-regexp
    "%g" 'dired-mark-files-containing-regexp
    "%m" 'dired-mark-files-regexp
    "%r" 'dired-do-rename-regexp
    "%C" 'dired-do-copy-regexp
    "%H" 'dired-do-hardlink-regexp
    "%R" 'dired-do-rename-regexp
    "%S" 'dired-do-symlink-regexp
    "%&" 'dired-flag-garbage-files
    ;; mark
    "*" nil
    "**" 'dired-mark-executables
    "*/" 'dired-mark-directories
    "*@" 'dired-mark-symlinks
    "*%" 'dired-mark-files-regexp
    "*(" 'dired-mark-sexp
    "*." 'dired-mark-extension
    "*O" 'dired-mark-omitted
    "*c" 'dired-change-marks
    "*s" 'dired-mark-subdir-files
    "*m" 'dired-mark
    "*u" 'dired-unmark
    "*?" 'dired-unmark-all-files
    "*!" 'dired-unmark-all-marks
    "U" 'dired-unmark-all-marks
    (kbd "* <delete>") 'dired-unmark-backward
    (kbd "* C-n") 'dired-next-marked-file
    (kbd "* C-p") 'dired-prev-marked-file
    "*t" 'dired-toggle-marks
    ;; Lower keys for commands not operating on all the marked files
    "a" 'dired-find-alternate-file
    "d" 'dired-flag-file-deletion
    "gf" 'dired-find-file
    (kbd "C-m") 'dired-find-file
    "gr" 'revert-buffer
    "i" 'dired-toggle-read-only
    "I" 'dired-maybe-insert-subdir
    "J" 'dired-goto-file
    "K" 'dired-do-kill-lines
    "r" 'dired-do-redisplay
    "m" 'dired-mark
    "t" 'dired-toggle-marks
    "u" 'dired-unmark                   ; also "*u"
    "W" 'browse-url-of-dired-file
    "x" 'dired-do-flagged-delete
    "gy" 'dired-show-file-type ;; FIXME: This could probably go on a better key.
    "Y" 'dired-copy-filename-as-kill
    "+" 'dired-create-directory
    ;; open
    (kbd "<return>") 'dired-find-file
    (kbd "S-<return>") 'dired-find-file-other-window
    (kbd "M-<return>") 'dired-display-file
    "gO" 'dired-find-file-other-window
    "go" 'dired-view-file
    ;; sort
    "o" 'dired-sort-toggle-or-edit
    ;; moving
    "gj" 'dired-next-dirline
    "gk" 'dired-prev-dirline
    "[[" 'dired-prev-dirline
    "]]" 'dired-next-dirline
    "<" 'dired-prev-dirline
    ">" 'dired-next-dirline
    "^" 'dired-up-directory
    " " 'dired-next-line
    [?\S-\ ] 'dired-previous-line
    [remap next-line] 'dired-next-line
    [remap previous-line] 'dired-previous-line
    ;; hiding
    "g$" 'dired-hide-subdir ;; FIXME: This can probably live on a better binding.
    (kbd "M-$") 'dired-hide-all
    "(" 'dired-hide-details-mode
    ;; isearch
    (kbd "M-s a C-s")   'dired-do-isearch
    (kbd "M-s a M-C-s") 'dired-do-isearch-regexp
    (kbd "M-s f C-s")   'dired-isearch-filenames
    (kbd "M-s f M-C-s") 'dired-isearch-filenames-regexp
    ;; misc
    [remap read-only-mode] 'dired-toggle-read-only
    ;; `toggle-read-only' is an obsolete alias for `read-only-mode'
    [remap toggle-read-only] 'dired-toggle-read-only
    "g?" 'dired-summary
    (kbd "<delete>") 'dired-unmark-backward
    [remap undo] 'dired-undo
    [remap advertised-undo] 'dired-undo
    ;; thumbnail manipulation (image-dired)
    (kbd "C-t d") 'image-dired-display-thumbs
    (kbd "C-t t") 'image-dired-tag-files
    (kbd "C-t r") 'image-dired-delete-tag
    (kbd "C-t j") 'image-dired-jump-thumbnail-buffer
    (kbd "C-t i") 'image-dired-dired-display-image
    (kbd "C-t x") 'image-dired-dired-display-external
    (kbd "C-t a") 'image-dired-display-thumbs-append
    (kbd "C-t .") 'image-dired-display-thumb
    (kbd "C-t c") 'image-dired-dired-comment-files
    (kbd "C-t f") 'image-dired-mark-tagged-files
    (kbd "C-t C-t") 'image-dired-dired-toggle-marked-thumbs
    (kbd "C-t e") 'image-dired-dired-edit-comment-and-tags
    ;; encryption and decryption (epa-dired)
    ";d" 'epa-dired-do-decrypt
    ";v" 'epa-dired-do-verify
    ";s" 'epa-dired-do-sign
    ";e" 'epa-dired-do-encrypt))

(provide 'ech-dired)
;;; ech-dired.el ends here
