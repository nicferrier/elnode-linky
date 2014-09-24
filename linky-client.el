;;; linky-client.el --- a client for linky.elnode.org -*- lexical-binding: t -*-

;; Copyright (C) 2014  Nic Ferrier

;; Author: Nic Ferrier <nferrier@ferrier.me.uk>
;; Keywords: hypermedia
;; Version: 0.0.2
;; Created: Fri Aug 29 23:07:28 BST 2014

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a curl based client for linky, a little HTTP-link tool
;; allowing you to post local links to an Internet site. This is
;; mostly useful if you're developing on multiple devices and you need
;; one to see the other without having to discover IPs (or indeed type
;; them in on fiddly mobile keyboards).

;; This client is based on sending a link to an elnode service being
;; hosted in your emacs, but may offer a generic interface in the
;; future, if I can be arsed. I have not packaged depended on elnode
;; because I may remove the dependency in the future.

;; The client sends requests to http://elnode.linky.org but you could
;; use any server, linky is available as a docker which you could just
;; run yourself.

;; See http://github.com/nicferrier/elnode-linky for more information
;; about linky, or
;; https://registry.hub.docker.com/u/nicferrier/elnode-linky for more
;; information about the docker.

;;; Code:

;;(require 'web)
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

;;;###autoload
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
              (let ((handler (elnode/con-lookup (cdr it) :elnode-http-handler)))
                (when (functionp handler) (documentation handler))))
             (car it)) 
            elnode-server-socket))
          (chosen
           (kva
            (completing-read "Elnode server: " completions)
            completions)))
     (list chosen
           (format "%s"
                   (let ((handler 
                          (elnode/con-lookup
                           (kva chosen elnode-server-socket)
                           :elnode-http-handler)))
                     (when (functionp handler) (documentation handler)))))))
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

(provide 'linky-client)

;;; linky-client.el ends here
