;; -*- lexical-binding: t; -*-;
;; My package configuration

 ;; package management
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("org" . "http://orgmode.org/elpa/")
        ("marmalade" . "http://marmalade-repo.org/")
        ("melpa" . "http://melpa.milkbox.net/packages/")))

(setq package-archive-enable-alist '(("gnu")
                                     ("marmalade")
                                     ("melpa"
                                      fill-column-indicator
                                      melpa)))
(setq package-archive-exclude-alist '(("gnu"
                                       fill-column-indicator
                                       melpa)
                                      ("marmalade"
                                       fill-column-indicator
                                       melpa)))


; activate all the packages (in particular autoloads)
(package-initialize)

; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

;; el-get packages
;; el-get kind of useful
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))
;; and the el-get sources
(setq
 el-get-sources
 '(el-get
   escreen
   php-mode-improved
   (:name smex                          ;; a better (ido like) M-x
          :after (lambda ()
                   (setq smex-save-file "~/.emacs.d/.smex-items")
                   (global-set-key (kbd "M-x") 'smex)
                   (global-set-key (kbd "M-X") 'smex-major-mode-commands)
                   ))
   (:name goto-last-change              ;; move pointer back to last change
          :after (lambda ()
                   (global-set-key (kbd "C-x C-/") 'goto-last-change)))))
(el-get 'sync)
