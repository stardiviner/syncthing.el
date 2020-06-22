;;; syncthing-api.el --- The API for Syncthing -*- lexical-binding: t; -*-

;;; Time-stamp: <2020-06-22 09:00:46 stardiviner>

;;; Commentary:



;;; Code:

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


(defmacro syncthing--rest-api (method endpoint &optional body &rest arglist)
  "The Syncthing REST API macro to construct HTTP METHOD, ENDPOINT with BODY and ARGLIST.
The generated functions like `syncthing-api-GET-/rest/system/browse'"
  `(defun ,(intern (format "syncthing-api-%s-%s" (eval method) (eval endpoint))) ,arglist
     (let ((url-request-method ,method)
           (url-request-extra-headers '(("X-API-Key" . ,syncthing-api-key))))
       (with-current-buffer
           (url-retrieve-synchronously
            (format "%s:%s%s" syncthing-server syncthing-port ,endpoint))
         (goto-char (point-min))
         (re-search-forward "^$")
         (let ((result (json-read)))
           result))
       ,@body)))

(defvar syncthing--http-rest-endpoints-alist
  '(;; System Endpoints
    ("GET" . "/rest/system/browse")
    ("GET" . "/rest/system/config")
    ("GET" . "/rest/system/config/isync")
    ("POST" . "/rest/system/config")
    ("GET" . "/rest/system/connections")
    ("GET" . "/rest/system/debug")
    ("POST" . "/rest/system/debug")
    ("GET" . "/rest/system/discovery")
    ("POST" . "/rest/system/discovery")
    ("POST" . "/rest/system/error/clear")
    ("GET" . "/rest/system/error")
    ("POST" . "/rest/system/error")
    ("GET" . "/rest/system/log")
    ("POST" . "/rest/system/pause")
    ("GET" . "/rest/system/ping")
    ("POST" . "/rest/system/ping")
    ("POST" . "/rest/system/reset")
    ("POST" . "/rest/system/restart")
    ("POST" . "/rest/system/resume")
    ("POST" . "/rest/system/shutdown")
    ("GET" . "/rest/system/status")
    ("GET" . "/rest/system/upgrade")
    ("POST" . "/rest/system/upgrade")
    ("GET" . "/rest/system/version")
    
    ;; Database Endpoints
    ("GET" . "/rest/db/browse")
    ("GET" . "/rest/db/completion")
    ("GET" . "/rest/db/file")
    ("GET" . "/rest/db/ignores")
    ("POST" . "/rest/db/ignores")
    ("GET" . "/rest/db/need")
    ("POST" . "/rest/db/override")
    ("POST" . "/rest/db/prio")
    ("POST" . "/rest/db/revert")
    ("POST" . "/rest/db/scan")
    ("GET" . "/rest/db/status")
    
    ;; Event Endpoints
    ("GET" . "/rest/events")

    ;; Statistics Endpoints
    ("GET" . "/rest/stats/device")
    ("GET" . "/rest/stats/folder")

    ;; Misc Services Endpoints
    ("GET" . "/rest/svc/deviceid")
    ("GET" . "/rest/svc/lang")
    ("GET" . "/rest/svc/random/string")
    ("GET" . "/rest/svc/report"))
  "Alist of Syncthing HTTP RESTful API endpoints")

(defvar syncthing--http-rest-endpoint nil
  "A temporary variable used in mapcar loop.")

(defun syncthing--http-rest-endpoints-initialize ()
  "Initialize HTTP RESTful endpoints API functions like `syncthing-api-GET-/rest/system/browse'."
  (mapcar
   (lambda (pair)
     (setq syncthing--http-rest-endpoint pair)
     ;; (syncthing--rest-api "GET" "/rest/system/config")
     (syncthing--rest-api (car syncthing--http-rest-endpoint) (cdr syncthing--http-rest-endpoint)))
   syncthing--http-rest-endpoints-alist))

(syncthing--http-rest-endpoints-initialize)



(provide 'syncthing-api)

;;; syncthing-api.el ends here
