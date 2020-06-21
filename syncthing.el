;;; syncthing.el --- Syncthing client for Emacs -*- lexical-binding: t; -*-

;;; Time-stamp: <2020-06-21 23:36:39 stardiviner>

;; Authors: stardiviner <numbchild@gmail.com>
;; Package-Requires: ((emacs "25.1"))
;; Package-Version: 0.1
;; Keywords: comm
;; homepage: https://github.com/stardiviner/syncthing.el

;; syncthing.el is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; syncthing.el is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Usage: [M-x syncthing]

;;; Code:

(defgroup syncthing nil
  "Syncthing client customize group."
  :prefix "syncthing-"
  :group 'syncthing)

(defcustom syncthing-server "http://localhost"
  "The server of Syncthing."
  :type 'string
  :safe #'stringp
  :group 'syncthing)

(defcustom syncthing-port 8080
  "The port of Syncthing."
  :type 'number
  :safe #'numberp
  :group 'syncthing)

(defcustom syncthing-api-key nil
  "The API key for Syncthing."
  :type 'string
  :safe #'stringp
  :group 'syncthing)


(defmacro syncthing--rest-api (method endpoint &rest arglist)
  "The Syncthing REST API macro to construct HTTP METHOD, ENDPOINT with ARGLIST."
  `(defun ,(intern (format "syncthing-api-%s-%s" method endpoint)) ,arglist
     (let ((url-request-method ,method)
           (url-request-extra-headers '(("X-API-Key" . ,syncthing-api-key))))
       (with-current-buffer
           (url-retrieve-synchronously
            (format "%s:%s%s" syncthing-server syncthing-port ,endpoint))
         (goto-char (point-min))
         (re-search-forward "^$")
         (let ((result (json-read)))
           result)))))




(provide 'syncthing)

;;; syncthing.el ends here
