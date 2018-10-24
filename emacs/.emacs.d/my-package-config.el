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

; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))
