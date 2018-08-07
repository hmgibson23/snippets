(defun docker-build--commits ()
  (car (split-string (completing-read "Pick commit:" (reverse (split-string
                                                               (shell-command-to-string "git log --pretty=oneline") "\n"))) " ")))


(defun docker-build--gono (sha ecr target)
  (interactive (list (docker-build--commits)
                     (read-string "ECR Name: ")
                     (read-string "Target: ")))
  (shell-command (format "docker build -t '429135341703.dkr.ecr.eu-west-1.amazonaws.com/%s:%s' -f './Dockerfile' '.' --build-arg main='simbasleep.io/%s' &" ecr sha target)))

(defun docker-pick-image ()
  (completing-read "Pick Simba Image: " (split-string (shell-command-to-string "docker images --format '{{.Repository}}:{{.Tag}}' | grep '429135341703'") "\n")))

(defun docker-push--gono (image)
  (interactive (list (docker-pick-image)))
  (ecr-login--dev)
  (shell-command (format "docker push %s &" image)))

(defun ecr-login--dev ()
  (interactive)
  (shell-command "eval $(aws ecr get-login --registry-ids=429135341703 --no-include-email | sed 's|https://||')"))


(defun kube-deploy--gono (env sha repo-name)
  (interactive (list (completing-read "Env: " '("dev" "stage" "prod"))
                     (docker-build--commits)
                     (read-string "Repo name: ")))

  (progn
    (setq arn-hash (make-hash-table :test 'equal))
    (setq command-string "CERT_ARN=%s ECR_REPO='429135341703.dkr.ecr.eu-west-1.amazonaws.com/%s:%s' SIMBA_ENV=%s envsubst < \"./k8s/manifest.yaml\" | kubectl apply -f -")
    (puthash "dev" "arn:aws:acm:eu-west-1:429135341703:certificate/42f31e86-17d8-4745-bddc-8542f9385ce3" arn-hash)
    (puthash "stage" "arn:aws:acm:eu-west-1:508584577282:certificate/6696721c-7b13-4d3e-a51b-fdf03ba7b6fd" arn-hash)
    (puthash "prod" "arn:aws:acm:eu-west-1:629605034269:certificate/6b613a90-2e8b-4b2c-a0a8-75a8f2ba61cd" arn-hash))

  (shell-command (format "kubectl config use-context main.k8s.%s.simbasleep.io" env))
  (shell-command (format command-string (gethash env arn-hash) repo-name sha env)))
  ;;
  ;; docker images --format '{{.Repository}}/{{.Tag}}' | grep '429135341703'
