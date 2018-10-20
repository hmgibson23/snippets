(defun my/eww-toggle-images ()
  "Toggle whether images are loaded and reload the current page fro cache."
  (interactive)
  (setq-local shr-inhibit-images (not shr-inhibit-images))
  (eww-reload t)
  (message "Images are now %s"
           (if shr-inhibit-images "off" "on")))

;; minimal rendering by default
(setq-default shr-inhibit-images t)
(setq-default shr-use-fonts nil)

(use-package gnus
  :config
  (defun switch-to-gnus (&optional arg)
    "Switch to a Gnus related buffer.
    Candidates are buffers starting with
     *mail or *reply or *wide reply
     *Summary or
     *Group*
    Use a prefix argument to start Gnus if no candidate exists."
    (interactive "P")
    (let (candidate
          (alist '(("^\\*\\(mail\\|\\(wide \\)?reply\\)" t)
                   ("^\\*Group")
                   ("^\\*Summary")
                   ("^\\*Article" nil (lambda ()
                                        (buffer-live-p gnus-article-current-summary))))))
      (catch 'none-found
        (dolist (item alist)
          (let (last
                (regexp (nth 0 item))
                (optional (nth 1 item))
                (test (nth 2 item)))
            (dolist (buf (buffer-list))
              (when (and (string-match regexp (buffer-name buf))
                         (> (buffer-size buf) 0))
                (setq last buf)))
            (cond ((and last (or (not test) (funcall test)))
                   (setq candidate last))
                  (optional
                   nil)
                  (t
                   (throw 'none-found t))))))
      (cond (candidate
             (switch-to-buffer candidate))
            (arg
             (gnus))
            (t
             (error "No candidate found")))))
  (setq
   user-mail-address "gibsonhugo@gmail.com"
   user-full-name "Hugo Gibson"
   gnus-select-method
   '(nnimap "gmail"
            (nnimap-address "imap.gmail.com")
            (nnimap-server-port 993)
            (nnimap-stream ssl))
   smtpmail-smtp-server "smtp.gmail.com"
   smtpmail-smtp-service 587
   message-send-mail-function 'smtpmail-send-it
   nntp-authinfo-file "~/.authinfo.gpg"
   gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]"
   gnus-agent nil
   gnus-message-archive-group nil)

  (add-hook 'gnus-startup-hook
            '(lambda ()
               (gnus-demon-init)
               (setq gnus-demon-timestep 60)  ;; each timestep is 60 seconds
               ;; Check for new mail every 1 timestep (1 minute)
               (gnus-demon-add-handler 'gnus-demon-scan-news 1 t)

               ;; Don't crash gnus if disconnected
               (defadvice gnus-demon-scan-news (around gnus-demon-timeout activate)
                 "Timeout for Gnus."
                 (with-timeout
                     (120 (message "Gnus timed out."))
                   ad-do-it))))
  (add-hook 'message-setup-hook 'mml-secure-message-encrypt)
  (add-hook 'gnus-summary-mode-hook 'my-gnus-summary-keys))

(defun my-gnus-summary-keys ()
  (local-set-key "y" 'gmail-archive)
  (local-set-key "$" 'gmail-report-spam))

(defun gmail-archive ()
  "Archive the current or marked mails.
This moves them into the All Mail folder."
  (interactive)
  (gnus-summary-move-article nil "nnimap+imap.gmail.com:[Gmail]/All Mail"))

(defun gmail-report-spam ()
  "Report the current or marked mails as spam.
This moves them into the Spam folder."
  (interactive)
  (gnus-summary-move-article nil "nnimap+imap.gmail.com:[Gmail]/Spam"))

(use-package elfeed
  :init
  (setq elfeed-feeds
        '("http://planet.emacsen.org/atom.xml"
          "http://fortnightlyreview.co.uk/feed/"
          "http://feeds.feedburner.com/TheParisReviewBlog"
          "https://cdn.lrb.co.uk/feeds/lrb"
          "https://www.thisiscolossal.com/feed/"
          "http://feeds.feedburner.com/zerohedge/feed"
          "https://lareviewofbooks.org/feed/?ver=2"
          "http://drawright.com/blog/?format=rss"
          "https://www.artistsnetwork.com/magazine/feed/"
          "http://www.thomasingmire.com/blog/feed"
          "http://www.brodyneuenschwander.com/feed/"
          "http://www.theartwolf.com/rss.xml"
          "https://www.bangkokpost.com/rss/data/topstories.xml"
          "https://wonky-words.com/feed/"
          "https://www.newcriterion.com/xml/lastissue.cfm"
          "https://www.newcriterion.com/xml/Latest.cfm"
          "http://www.private-eye.co.uk/rss/rss.php"
          "https://artandinfluence.com/feed"
          "https://www.art-agenda.com/category/reviews/feed/"
          "https://feeds.feedburner.com/efluxjournal?format=xml/"
          "https://commercialtype.com/news/rss")))

(use-package gmail2bbdb)
(use-package erc
  :ensure nil
  :defer t
  :config
  (add-hook 'window-configuration-change-hook
            (lambda ()
              (setq erc-fill-column (- (window-width) 2))))
  (add-hook 'erc-mode-hook
            (lambda ()
              (setq-local scroll-margin 1)))

  (setq erc-rename-buffers t
        erc-interpret-mirc-color t
        erc-lurker-hide-list '("JOIN" "PART" "QUIT")
        erc-autojoin-channels-alist '(("freenode.net" "#emacs"))))
