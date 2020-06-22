;;; syncthing.el --- Syncthing client for Emacs -*- lexical-binding: t; -*-

;;; Time-stamp: <2020-06-22 08:58:29 stardiviner>

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

(require 'syncthing-api)


(defun syncthing--connection-extract-info (connection)
  "Extract CONNECTION into for Elisp."
  (let ((connection-id (car connection))
        (address (alist-get 'address connection))
        (at (alist-get 'at connection))
        (client-version (alist-get 'clientVersion connection))
        (connected (alist-get 'connected connection))
        (crypto (alist-get 'crypto connection))
        (in-bytes-total (alist-get 'inBytesTotal connection))
        (out-bytes-total (alist-get 'outBytesTotal connection))
        (paused (alist-get 'paused connection))
        (type (alist-get 'type connection)))
    ;; TODO
    connection-id))

(defun syncthing-connection-list ()
  "List of Syncthing connections."
  ;; total
  (syncthing--connection-extract-info
   (assoc 'total (syncthing-api-GET-/rest/system/connections)))
  
  (mapcar
   (lambda (connection)
     (syncthing--connection-extract-info connection))
   (alist-get 'connections (syncthing-api-GET-/rest/system/connections))))



(provide 'syncthing)

;;; syncthing.el ends here
