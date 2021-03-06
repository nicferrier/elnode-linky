;;; linky.el --- a simple link post app for help with testing on LANs -*- lexical-binding: t -*-

;; Copyright (C) 2014  Nic Ferrier

;; Author: Nic Ferrier <nferrier@ferrier.me.uk>
;; Keywords: hypermedia

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

;; A very simple app for solving the problem of how to find links
;; you're hosting on your local laptop.

;;; Code:

(require 'elnode)
(require 'db)

(defvar linky/auth-db   ;; this needs to be stored in a docker volume
  (db-make
   `(db-hash
     :filename ,(let ((file
                       (expand-file-name "db/auth-db" default-directory)))
                     (make-directory (file-name-directory file) t)
                     file)))
  "The db where we store user information.")

(elnode-defauth :linky-auth
 :cookie-name "linky-auth"
 :auth-db linky/auth-db)

(defconst linky/store (make-hash-table :test 'equal)
  "Store, keyed by username; value is a list of links.

The list of links is an alist: HREF . EXTENDED-INFO where
EXTENDED-INFO is a list whose first element is an optional name
for the tag.  Thus the alist may have a `nil' CDR.

Other information may be added to the EXTENDED-INFO.")

(defconst linky/form-html "<form method=\"POST\" action=\"/\">
<legend>add a new address</legend>
<input type=\"text\" name=\"a\" placeholder=\"address\"></input><br></br>
<input type=\"text\" name=\"n\" placeholder=\"name\"></input><br></br>
<input type=\"submit\" name=\"submit\"></input>
</form>")

(defun linky/store->html (username)
  (let ((details (gethash username linky/store)))
    (format "<!doctype html>
<html>
<head>
<title>linky!</title>
<meta name=\"viewport\" content=\"width=450,user-scalable=no\"></meta>
<meta charset=\"utf-8\"></meta>
<link rel=\"stylesheet\" href=\"/-/style.css\" type=\"text/css\"></link>
</head>
<body>
<a href=\"https://github.com/nicferrier/elnode-linky\"><img style=\"position: absolute; top: 0; right: 0; border: 0;\" src=\"https://camo.githubusercontent.com/a6677b08c955af8400f44c6298f40e7d19cc5b2d/68747470733a2f2f73332e616d617a6f6e6177732e636f6d2f6769746875622f726962626f6e732f666f726b6d655f72696768745f677261795f3664366436642e706e67\" alt=\"Fork me on GitHub\" data-canonical-src=\"https://s3.amazonaws.com/github/ribbons/forkme_right_gray_6d6d6d.png\"></a>
<h1>linky</h1>
<ul>%s</ul>
%s
</body></html>"
            (->> details
              (--map (format
                      "<li><a href=\"%s\">%s %s</a></li>"
                      (car it)
                      (or (cadr it) (car it))
                      (if (car it)
                          (format "<span class=\"url\">[%s]</span>" (car it))
                          "")))
              (s-join "\n"))
            linky/form-html)))

(defun linky/store-add (username address name)
  "Add a site record for the USERNAME to `linky/store'.

ADDRESS is the address of the record.  NAME is an optional name
to describe it.  If not given then we just use the ADDRESS."
  (let ((record (gethash username linky/store)))
    (puthash username
             (cons
              (cons address (list (or name address)))
              record)
             linky/store)))

(defun linky/store-add-user (username password)
  (if (db-get username linky/auth-db)
      :error
      ;; Else...
      (elnode-auth-user-add username password 'linky/auth-db)))

(defun linky/register (httpcon)
  (let ((username (elnode-http-param httpcon "username"))
        (password (elnode-http-param httpcon "password")))
    (if (not (and username password))
        (elnode-send-redirect httpcon "/")
        ;; Else save the user
        (linky/store-add-user username password)
        (elnode-send-redirect httpcon "/"))))

(defun linky (httpcon)
  (with-elnode-auth httpcon :linky-auth
    (let ((me (elnode-auth-username httpcon)))
      (elnode-method httpcon
        (GET (elnode-send-html httpcon (linky/store->html me)))
        (POST
         (let* ((address (elnode-http-param httpcon "a"))
                (name (or (elnode-http-param httpcon "n") address)))
           (linky/store-add me address name)
           (elnode-send-redirect httpcon "/")))))))

(defconst linky/docroot (expand-file-name "docroot")
  "Where we put static files.")

(defun linky/ws (httpcon)
  "Webserver for linky."
  (elnode--webserver-handler-proc
   httpcon
   linky/docroot elnode-webserver-extra-mimetypes))

(defun linky-router (httpcon)
  "Simple router for `linky'."
  (elnode-dispatcher
   httpcon
   '(("^/-/\\(.*\\)" . linky/ws)
     ("^/register/" . linky/register)
     ("^/$" . linky))
   :auth-scheme :linky-auth))

(elnode-start
 'linky-router
 :port 8005
 :host "0.0.0.0")

(provide 'linky)

;;; linky.el ends here
