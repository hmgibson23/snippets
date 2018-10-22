
;; -*- lexical-binding: t; -*-;

(use-package slack
  :commands (slack-start)
  :init
  (setq slack-buffer-emojify t) ;; if you want to enable emoji, default nil
  (setq slack-prefer-current-team t)
  :config
  (slack-register-team
   :name "simba-sleep"
   :default t
   :full-and-display-names t
   :client-id (auth-source-search :host "slack.com"
                                      :requires '(secret))
   :client-secret (auth-source-search :host "slack.com"
                                      :requires '(user))
   :token "xoxs-21114220564-339565068272-426142849520-3cb8e0be634f24cff609a8f7ef4c08f3f181bc49badf565b4c9ff8c05c811991"

   ;; (auth-source-search :host "slack.token"
   ;;                     :requires '(secret))
   :subscribed-channels '(core technology tech-contractors))

  ;; turn this into a hydra
  ;; (evil-define-key 'normal slack-info-mode-map
  ;;   ",u" 'slack-room-update-messages)
  ;; (evil-define-key 'normal slack-mode-map
  ;;   ",c" 'slack-buffer-kill
  ;;   ",ra" 'slack-message-add-reaction
  ;;   ",rr" 'slack-message-remove-reaction
  ;;   ",rs" 'slack-message-show-reaction-users
  ;;   ",pl" 'slack-room-pins-list
  ;;   ",pa" 'slack-message-pins-add
  ;;   ",pr" 'slack-message-pins-remove
  ;;   ",mm" 'slack-message-write-another-buffer
  ;;   ",me" 'slack-message-edit
  ;;   ",md" 'slack-message-delete
  ;;   ",u" 'slack-room-update-messages
  ;;   ",2" 'slack-message-embed-mention
  ;;   ",3" 'slack-message-embed-channel
  ;;   "\C-n" 'slack-buffer-goto-next-message
  ;;   "\C-p" 'slack-buffer-goto-prev-message)
  ;;  (evil-define-key 'normal slack-edit-message-mode-map
  ;;   ",k" 'slack-message-cancel-edit
  ;;   ",s" 'slack-message-send-from-buffer
  ;;   ",2" 'slack-message-embed-mention
  ;;   ",3" 'slack-message-embed-channel)
   )


(use-package alert
  :commands (alert)
  :init
  (setq alert-default-style 'notifier))
