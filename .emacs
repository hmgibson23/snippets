
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)
(pallet-mode t)

(load "~/.emacs.d/my-functions")
(load "~/.emacs.d/my-key-bindings")
(load "~/.emacs.d/my-system-config")
(load "~/.emacs.d/init")
(require 're-builder)
(setq reb-re-syntax 'string)
(global-undo-tree-mode 1)

;; kinda useful to have the uncommitted shizz available
(global-diff-hl-mode)

;; general pretty looking stuff
(menu-bar-mode 0)
(tool-bar-mode -1)
(fset 'yes-or-no-p 'y-or-n-p)
(setq confirm-nonexistent-file-or-buffer nil)
(size-indication-mode t)
(line-number-mode t)
(column-number-mode t)
(display-time-mode)
(setq tab-always-indent 'complete)
(add-to-list 'completion-styles 'initials t)
(setq-default indent-tabs-mode nil
              indicate-buffer-boundaries 'left
              indicate-empty-lines t)
(setq-default tab-width 4)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(global-linum-mode t)
(show-paren-mode 1)
(global-auto-revert-mode 1)

;; Electric indent drives me nuts
(electric-indent-mode -1)

;;;;;;;;;;;;;;;;;;;;
;; UTF-8
;;;;;;;;;;;;;;;;;;;;
;; set up unicode
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(defalias 'yes-or-nop 'y-or-no-p)

;; I can spell. Make no mistake
;; I'm a serious speller, like no
;; other but I'm lazy too so this
;; is included
(setq flyspell-issue-welcome-flag nil)
(setq-default ispell-program-name "/usr/bin/aspell")
(setq-default ispell-list-command "list")
;;temp directory for flymaking etc                                                       .
(setq temporary-file-directory "~/.emacs.d/tmp/")

(setq confirm-nonexistent-file-or-buffer nil)

(setq tramp-default-method "scp")
;; get rid of `find-file-read-only' and replace it with something
;; more useful.
(global-set-key (kbd "C-x C-r") 'ido-recentf-open)
;; enable recent files mode.

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-revert-remote-files t)
 '(company-ghc-show-info t t)
 '(company-quickhelp-mode t)
 '(custom-safe-themes
   (quote
    ("bf390ecb203806cbe351b966a88fc3036f3ff68cd2547db6ee3676e87327b311" "1195d71dfd46c43492993a528336ac7f8c7400b4c58338e5b40329d6cad655b6" "725a0ac226fc6a7372074c8924c18394448bb011916c05a87518ad4563738668" "04589c18c2087cd6f12c01807eed0bdaa63983787025c209b89c779c61c3a4c4" "b550fc3d6f0407185ace746913449f6ed5ddc4a9f0cf3be218af4fb3127c7877" "0b7ee9bac81558c11000b65100f29b09488ff9182c083fb303c7f13fd0ec8d2b" "f440c56837fe91555d819a82ee1770fdbd81153928aba953166f4fe1b0c4d4f1" "38e66a2a20fa9a27af5ffc4f4dd54f69e3fef6b51be7b351e137b24958bfebd7" "cc60d17db31a53adf93ec6fad5a9cfff6e177664994a52346f81f62840fe8e23" "01ce486c3a7c8b37cf13f8c95ca4bb3c11413228b35676025fdf239e77019ea1" "dc65b57fa51eb557725aaf7fd6d52d1b8cfe6ed7d154a9769b376c594447a12c" "9d91458c4ad7c74cf946bd97ad085c0f6a40c370ac0a1cbeb2e3879f15b40553" "f5ef7ddecf161a2951048c204c2c6d9d5be08745b136dce583056ad4b234b861" "118717ce0a2645a0cf240b044999f964577ee10137b1f992b09a317d5073c02d" "f7b2af67ec4434c94a3869dc1f0e4d2a4f85e5a1bec2b1c19776717d7ad001da" "a507b9ca4a605d5256716da70961741b9ef9ec3246041a4eb776102e8df18418" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "a772eafba4eda0ed57a5d651a96804487a1dacbfbf8658084bfe84546a7c7008" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "756597b162f1be60a12dbd52bab71d40d6a2845a3e3c2584c6573ee9c332a66e" "6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" "c7359bd375132044fe993562dfa736ae79efc620f68bab36bd686430c980df1c" "e26780280b5248eb9b2d02a237d9941956fc94972443b0f7aeec12b5c15db9f3" "33c5a452a4095f7e4f6746b66f322ef6da0e770b76c0ed98a438e76c497040bb" "a774c5551bc56d7a9c362dca4d73a374582caedb110c201a09b410c0ebbb5e70" default)))
 '(edts-man-root "/Users/hgibson/.emacs.d/edts/doc/R16B03")
 '(ensime-default-java-flags (quote ("-Xmx4025m")))
 '(font-lock-add-keywords (quote clojure-mode) t)
 '(global-anzu-mode t)
 '(grep-find-ignored-files
   (quote
    ("TABS" ".#*" "*.hi" "*.cmti" "*.cmt" "*.annot" "*.cmi" "*.cmxa" "*.cma" "*.cmx" "*.cmo" "*.o" "*~" "*.bin" "*.lbin" "*.so" "*.a" "*.ln" "*.blg" "*.bbl" "*.elc" "*.lof" "*.glo" "*.idx" "*.lot" "*.fmt" "*.tfm" "*.class" "*.fas" "*.lib" "*.mem" "*.x86f" "*.sparcf" "*.dfsl" "*.pfsl" "*.d64fsl" "*.p64fsl" "*.lx64fsl" "*.lx32fsl" "*.dx64fsl" "*.dx32fsl" "*.fx64fsl" "*.fx32fsl" "*.sx64fsl" "*.sx32fsl" "*.wx64fsl" "*.wx32fsl" "*.fasl" "*.ufsl" "*.fsl" "*.dxl" "*.lo" "*.la" "*.gmo" "*.mo" "*.toc" "*.aux" "*.cp" "*.fn" "*.ky" "*.pg" "*.tp" "*.vr" "*.cps" "*.fns" "*.kys" "*.pgs" "*.tps" "*.vrs" "*.pyc" "*.pyo")))
 '(hackernews-items-per-page 30)
 '(haskell-complete-module-preferred
   (quote
    ("Data.ByteString" "Data.ByteString.Lazy" "Data.Conduit" "Data.Function" "Data.List" "Data.Map" "Data.Maybe" "Data.Monoid" "Data.Ord")) t)
 '(haskell-interactive-popup-errors nil)
 '(haskell-tags-on-save t t)
 '(insert-shebang-file-types
   (quote
    (("py" . "python")
     ("groovy" . "groovy")
     ("fish" . "fish")
     ("robot" . "robot")
     ("rb" . "ruby")
     ("lua" . "lua")
     ("php" . "php")
     ("sh" . "bash")
     ("pl" . "perl")
     ("bats" . "bats"))))
 '(ivy-mode t)
 '(max-specpdl-size 3000)
 '(muse-project-alist nil)
 '(package-selected-packages
   (quote
    (company-irony-c-headers nasm-mode nord-theme ivy magit pythonic sesman typescript-mode rustic rust-playground evil-avy evil-colemak-basics colemak-evil evil-dvorak xresources-theme evil-numbers evil-ledger evil-expat evil-magit sentence-navigation evil-mu4e sql-indent flymake-json company-rtags ledger-mode uuidgen uuid general evil-goggles evil-exchange evil-args evil-anzu evil-surround evil-mc evil-paredit evil-tutor evil-collection company-inf-ruby robe poporg fontawesome vue-mode vue-html-mode docker-compose-mode wanderlust imenu-list imenu-anywhere dired-imenu gnuplot-mode gnuplot dired-narrow dired-rsync diredfl fzf quickrun corral evil flycheck-checkbashisms writeroom-mode writegood-mode muse flycheck-nim nim-mode go-dlv go-fill-struct go-impl go-imports bshell calfw calfw-gcal bpr all-the-icons-ivy counsel-world-clock counsel-projectile ivy-hydra sx gorepl-mode restclient flycheck-yamllint go-playground go-direx go-gopath go-stacktracer go-add-tags go-projectile gotest go-eldoc timonier forecast cask-mode caskxy key-chord sunshine kubernetes gmail2bbdb mastodon elfeed yahoo-weather weather-metno expand-region window-numbering clojure-cheatsheet clojure-quick-repls closql command-queue commenter flymake-elixir npm-mode use-package shell-switcher dired-ranger dired-collapse prettier-js xterm-color yarn-mode flycheck-elixir bats-mode alchemist elixir-mode toml-mode dictionary company-lua flow-minor-mode mode-icons pdf-tools howdoi company-terraform terraform-mode yaml-mode mustache-mode gdscript-mode lua-mode tide xref-js2 nodejs-repl pandoc pandoc-mode kotlin-mode async git-commit ht irony js2-mode magit-popup package-build s seq simple-httpd tern makefile-executor sml-mode sml-modeline insert-shebang org-projectile handlebars-mode indium less-css-mode hackernews ace-window eslintd-fix editorconfig rjsx-mode madhat2r-theme goose-theme counsel ido-vertical-mode helm avy aggressive-indent ibuffer-projectile ibuffer-vc company-go go-mode reykjavik-theme esup slack google-this git-timemachine git-messenger prodigy git-gutter eslint-fix html-script-src import-js jsfmt tj-mode web-beautify ctags-update company-anaconda tern-context-coloring jsx-mode js2-refactor js2-highlight-vars flycheck-pos-tip clojure-mode color-theme docker-tramp edn find-file-in-project ghc haskell-mode highlight-indentation hydra inf-ruby inflections jedi-core json-mode json-reformat json-snatcher marshal multiple-cursors paredit pcache peg pos-tip python-environment pyvenv queue request-deferred sbt-mode scala-mode skewer-mode spinner tablist web-completion-data websocket with-editor yasnippet pallet js2-closure jade javascript zenburn-theme yari yard-mode wgrep web-mode virtualenvwrapper virtualenv utop undo-tree tuareg top-mode tern-auto-complete swiper subatomic-theme ssh soothe-theme smex smartparens smart-mode-line slime scss-mode rust-mode ruby-hash-syntax ruby-end rtags rspec-mode rbenv rainbow-delimiters racket-mode quack projectile persistent-scratch page-break-lines org-trello omnisharp nvm multi-term merlin markdown-preview-eww markdown-mode julia-shell js-comint jedi irony-eldoc ipython inf-mongo igrep iedit hindent gotham-theme gist ghci-completion ghc-imported-from geiser fsharp-mode flymd flymake-jslint flymake-jshint flymake-hlint flymake-cursor flylisp flycheck-tip flycheck-rust flycheck-irony flx-ido exec-path-from-shell ensime enh-ruby-mode elpy elein dockerfile-mode docker csv-mode company-web company-tern company-jedi company-irony company-ghci company-ghc company-cabal company-c-headers color-theme-monokai color-theme-molokai clojurescript-mode clj-refactor circe cherry-blossom-theme bundler buffer-move browse-kill-ring bbdb-vcard bash-completion awk-it autopair anzu ag ace-jump-mode ac-js2 ac-inf-ruby ac-cider)))
 '(quack-programs
   (quote
    ("/usr/bin/local/csi" "/usr/local/bin/csi" "bigloo" "chicken" "csi" "csi -hygienic" "gosh" "gracket" "gsi" "gsi ~~/syntax-case.scm -" "guile" "kawa" "mit-scheme" "racket" "racket -il typed/racket" "rs" "scheme" "scheme48" "scsh" "sisc" "stklos" "sxi")))
 '(send-mail-function (quote smtpmail-send-it))
 '(shell-pop-universal-key "C-t")
 '(sql-postgres-login-params
   (quote
    ((user :default "hugo")
     server
     (database :default "hugo")
     port)))
 '(virtualenv-root "/home/hugo"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-goggles-change-face ((t (:inherit diff-removed))))
 '(evil-goggles-delete-face ((t (:inherit diff-removed))))
 '(evil-goggles-paste-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-add-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-change-face ((t (:inherit diff-changed))))
 '(evil-goggles-undo-redo-remove-face ((t (:inherit diff-removed))))
 '(evil-goggles-yank-face ((t (:inherit diff-changed))))
 '(flymake-error ((((class color)) (:underline "red"))))
 '(flymake-warning ((((class color)) (:underline "yellow")))))
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;; change default working directory - different for them all.
(setq-default default-directory "~/")


;; use magic to set the exec path
;; (set-exec-path-from-shell-PATH)
(setq ring-bell-function 'ignore)

;; whatever - just save and compile
(setq compilation-ask-about-save nil)

;; enable for all programming modes -- allows camel case editing
(add-hook 'prog-mode-hook 'subword-mode)

;; persistent scratch - me like
(persistent-scratch-setup-default)
(persistent-scratch-autosave-mode 1)


;; dired listing switches for fun times
(setq dired-listing-switches "-lash")

;; silence the ad warnings
(setq ad-redefinition-action 'accept)

;; enable editorconfig
(editorconfig-mode 1)

(exec-path-from-shell-initialize)
(setq browse-url-browser-function 'eww-browse-url)

(toggle-scroll-bar -1)
