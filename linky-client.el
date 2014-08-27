;;; A little client for linky

(require 'web)
(require 'rx)

(defun linky-web-client ()
  ;; FIXME!!! this is not working because web can't remember cookies
  (web-http-post
   (lambda (con hdr data)
     (web-cookie-handler
      con hdr data
      (lambda (con hdr data)
        )))
   :url "http://linky.elnode.org/login/"
   :data '(("username" . "nicferrier")
           ("password" . "cushions"))))


(defvar linky-curl/username-history nil
  "History for usernames.")

(defun linky-curl (port name &optional username password)
  "A simple linky client.

Interactively it will complete a port and name from the
`elnode-server-socket' and read USERNAME and PASSWORD from the
minibuffer.

Non-interactively you must pass USERNAME and PASSWORD for it to
work."
  (interactive
   ;; Return the port of the server
   (let* ((completions
           (--keep
            (cons
             (format
              "%d %s"
              (car it)
              (elnode/con-lookup (cdr it) :elnode-http-handler)) (car it)) 
            elnode-server-socket))
          (chosen
           (kva
            (completing-read "Elnode server: " completions)
            completions)))
     (list chosen
           (format "%s"
                   (elnode/con-lookup
                    (kva chosen elnode-server-socket)
                    :elnode-http-handler)))))
  (noflet ((curl (url cookie-flag data) ; a function to do curl
             (let* ((curl-cmd
                     (format
                      "curl -f -s -d %s -%s %s http://linky.elnode.org%s"
                      data
                      cookie-flag
                      (expand-file-name "~/.linky")
                      url))
                    (curl-args (split-string curl-cmd " ")))
               (with-temp-buffer
                 (apply 'call-process "curl" nil t nil (cdr curl-args))
                 (s-trim (buffer-string))))))
    (let* ((addr
            (format
             "http://%s:%d"
             (s-trim 
              (shell-command-to-string
               "ip addr show wlan0 | sed -rne 's|.*inet (.*)/.*|\\1|p'"))
             port))
           (post-data
            (web-to-query-string
             `(("a" . ,addr)
               ("n" . ,name)))))
      (unless (string-match-p
               (curl "/" "b" post-data)
               "<h1>redirecting you to /</h1>")
        ;; Try and login to do it again...
        (let* ((data
                (apply
                 'format "username=%s&password=%s"
                 (if (called-interactively-p 'interactive)
                     (list
                      (read-from-minibuffer
                       "username: " nil nil nil
                       'linky-curl/username-history)
                      (read-passwd "password: "))
                     (if (and username password)
                         (list username password)
                         (error "linky: you must supply a username and password"))))))
          (if (string-match-p
               (curl "/login/" "c" data)
               "<h1>redirecting you to /</h1>")
              (curl "/" "b" post-data)
              ;; Else...
              (error "linky: login failed!")))))))

;;; linky-client.el ends here
