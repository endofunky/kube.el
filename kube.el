;;; -*- lexical-binding: t; -*-
(require 'async)
(require 'parse-time)
(require 'transient)
(require 'kubernetes-tramp)
(require 'term)
(require 'evil nil t)
(with-no-warnings
  (require 'cl))

(setq lexical-binding t)

(defun kube--run (&rest xs)
  (string-trim
   (shell-command-to-string (string-join (cons "kubectl" xs) " "))))

(defun kube--contexts ()
  (split-string (kube--run "config get-contexts -o name")))

(defun kube-context ()
  (kube--run "config current-context"))

(defun kube-use-context (ctx)
  (interactive (list (completing-read "Context: " (kube--contexts))))
  (message (kube--run "config use-context" ctx))
  (kube-refresh))

(defun kube-get-pods (&optional flags)
  (let ((json-array-type 'list))
    (alist-get 'items (json-read-from-string
                       (kube--run "get pods -o json" flags)))))

(defsubst kube-pod->container-statuses (pod)
  (let ((status (alist-get 'status pod)))
    (alist-get 'containerStatuses status)))

(defun kube-pod->age (pod)
  (let* ((status (alist-get 'status pod))
         (start (alist-get 'startTime status)))
    (kube-utils-time-diff-string (parse-iso8601-time-string start)
                                 (current-time))))

(defun kube-utils-time-diff-string (start now)
  "Find the interval between START and NOW, and return a string of the coarsest unit."
  (let ((diff (time-to-seconds (time-subtract now start))))
    (car (split-string (format-seconds "%yy,%dd,%hh,%mm,%ss%z" diff) ","))))

(defun kube-pod->tabulated-list-entry (pod)
  (let ((metadata (assoc 'metadata pod))
        (status (assoc 'status pod)))
    (list (alist-get 'name metadata)
          (vector
           ;; Pod name
           (alist-get 'name metadata)
           ;; Namespace
           (alist-get 'namespace metadata)
           ;; Status
           (alist-get 'phase status)
           ;; Restarts
           (format "%d" (cl-reduce #'(lambda (x cs)
                                       (+ x (alist-get 'restartCount cs)))
                                   (kube-pod->container-statuses pod)
                                   :initial-value 0))
           ;; Running
           (format "%d/%d" (cl-count-if '(lambda (cs) (alist-get 'ready cs))
                                        (kube-pod->container-statuses pod))
                   (length (kube-pod->container-statuses pod)))
           ;; Age
           (kube-pod->age pod)))))

(defun kube-pods->tabulated-list-entries (pods)
  (mapcar #'kube-pod->tabulated-list-entry pods))

(defun kube-visit ()
  (interactive)
  (message (concat "current line ID is: " (tabulated-list-get-id))))

(defun kube-pod ()
  (interactive)
  (pop-to-buffer "*kube*" nil)
  (kube-mode)
  (kube-refresh))

(defalias 'kube #'kube-pod)

(defun kube--read-file (f)
  (with-temp-buffer
    (insert-file-contents f)
    (buffer-string)))

(defun kube-pod-dired ()
  (interactive)
  (dired (format "/kubectl:%s:/" (tabulated-list-get-id))))

(defun kube-refresh ()
  (interactive)
  (message "Refreshing pods ...")
  (lexical-let ((kbuf (get-buffer "*kube*"))
                (f (make-temp-file "kubectl-json"))
                (buf (generate-new-buffer " *kubectl*")))
    (set-process-sentinel
     (start-process "kubectl"
                    buf
                    shell-file-name
                    shell-command-switch
                    (concat "kubectl get pods -o json > " f))
     (lambda (process _signal)
       (when (memq (process-status process) '(exit signal))
         (with-current-buffer kbuf
           (setq tabulated-list-entries
                 (kube-pods->tabulated-list-entries
                  (alist-get 'items (json-read-from-string (kube--read-file f)))))
           (tabulated-list-print t))
         (delete-file f)
         (kill-buffer buf)
         (message "Pods refreshed."))))))

(defun kube-run-command (cmd)
  (interactive (list (read-from-minibuffer "Command: " "/bin/bash")))
  (let* ((program "kubectl")
         (switches (append (list "exec" "-it" (tabulated-list-get-id))
                           (split-string cmd)))
         (name (string-join (cons program switches) " "))
         (buffer (get-buffer-create (concat "*" name "*"))))
    (cond ((not (term-check-proc buffer))
	   (with-current-buffer buffer
	     (term-mode))
	   (term-exec buffer name program nil switches)))
    (pop-to-buffer buffer)))

(define-infix-argument kube-pod-delete:--all ()
  :description "Delete all resources"
  :class 'transient-switch
  :argument "--all")

(define-infix-argument kube-pod-delete:--cascade ()
  :description "Cascade deletion"
  :class 'transient-option
  :argument "--cascade="
  :choices '("true" "false"))

(define-infix-argument kube-pod-delete:--now ()
  :description "Immediate shutdown"
  :class 'transient-switch
  :argument "--now")

(define-infix-argument kube-pod-delete:--force ()
  :description "Immediate deletion of resources"
  :class 'transient-switch
  :argument "--force")

(define-suffix-command kube-pod-delete-run (args)
  (interactive (list (transient-args 'kube-pod-delete)))
  (message
   (if (member "--all" args)
       (kube--run "delete pod" (string-join args " "))
     (kube--run "delete pod" (tabulated-list-get-id) (string-join args " ")))))

(define-transient-command kube-pod-delete ()
  "Delete pods."
  :value '("--cascade=true")
  ["Arguments"
   ("-a" kube-pod-delete:--all)
   ("-f" kube-pod-delete:--force)
   ("-n" kube-pod-delete:--now)
   ("-c" kube-pod-delete:--cascade)]
  ["Actions"
   ("D" "Delete" kube-pod-delete-run)])

(define-transient-command kube-dispatch ()
  "Invoke a Kube command from a list of available commands."
  ["Actions"
   [("c"        "Switch context" kube-use-context)
    ("x"        "Execute command" kube-run-command)]
   [("D"        "Delete pod" kube-pod-delete)]]
  ["TRAMP"
   ("d"        "dired" kube-pod-dired)]
  ["Commands"
   ("r"        "Refresh" kube-refresh)
   ("<return>" "visit thing at point" kube-visit)
   ("C-h m"    "show all key bindings" describe-mode)])

(defvar kube-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'kube-visit)
    (define-key map (kbd "c") 'kube-use-context)
    (define-key map (kbd "r") 'kube-refresh)
    (define-key map (kbd "d") 'kube-pod-dired)
    (define-key map (kbd "D") 'kube-pod-delete)
    (define-key map (kbd "x") 'kube-run-command)
    (define-key map (kbd "?") 'kube-dispatch)
    map)
  "Keymap for `kube-mode'.")

(define-derived-mode kube-mode tabulated-list-mode "kube" "Kube"
  (buffer-disable-undo)
  (kill-all-local-variables)
  (setq truncate-lines t)
  (setq mode-name "Kube")
  (setq major-mode 'kube-mode)
  (use-local-map kube-mode-map)
  (hl-line-mode t)

  (eval-after-load 'evil-mode
    (evil-add-hjkl-bindings kube-mode-map 'normal
      (kbd "RET") 'kube-visit
      (kbd "?") 'kube-dispatch
      (kbd "c") 'kube-use-context
      (kbd "d") 'kube-pod-dired
      (kbd "D") 'kube-pod-delete
      (kbd "x") 'kube-run-command
      (kbd "r") 'kube-refresh))

  (setq tabulated-list-format [("NAME" 45 t)
                               ("NAMESPACE" 10 t)
                               ("STATUS" 10 t)
                               ("RESTARTS" 9 t)
                               ("READY" 5 t)
                               ("AGE" 0 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "NAME" nil))
  (tabulated-list-init-header)
  (tabulated-list-print))

(provide 'kube)
