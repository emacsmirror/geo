;;; geo-ip.el --- Geolocation services for GNU Emacs -*- lexical-binding: t; -*-

;; Copyright (c) 2020 Would (oldosfan).

;; This file is not part of GNU Emacs.

;; geo-ip.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; geo-ip.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with geo-ip.el. If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;;; A small wrapper for geoip-db.com.

;;; Code:

(require 'geo)

(defvar geo-ip--last-callback-success-p nil
  "Whether or not the last callback was successful.")

(defvar geo-ip--last-location nil
  "The last location provided by geo-ip.")

(defvar geo-ip--changed-hook nil
  "Callbacks to be run when the IP geolocation is changed.")

(defvar geo-ip-urls '("https://geoip-db.com/json/"
		      "https://freegeoip.app/json/")
  "A list of URLs to be used by geo-ip.
Entries can either be a string, or a pair of a URL and a quoted
function that accept a JSON object, and return a cons pair of
latitude and longitude.")

(defun geo-ip--get-url (entry)
  "Retrieve the URL from ENTRY."
  (cond ((stringp entry) entry)
	((consp entry) (car entry))))

(defun geo-ip--extract-latl (entry json)
  "Retrieve the latitude and longitude from ENTRY with JSON."
  (if (stringp entry)
      (cons (cdr (assq 'latitude json))
	    (cdr (assq 'longitude json)))
    (funcall (cdr entry) json)))

(defun geo-ip--async-retrieve-ip (callback)
  "Call CALLBACK with the current device's latitude, if it exists."
  (ignore-errors
    (setq geo-ip--last-callback-success-p nil)
    (async-start `(lambda ()
		    (require 'url)
		    (require 'json)
		    (eval '(letrec ((l (lambda (urls)
					 (condition-case nil
					     (progn
					       (with-current-buffer (url-retrieve-synchronously
								     (funcall ,(list 'quote
										     (symbol-function #'geo-ip--get-url))
									      (car urls)))
						 (goto-char (point-min))
						 (search-forward "{")
						 (previous-line)
						 (delete-region (point-min) (point))
						 (cons (json-read) (car urls))))
					   (error (when (cdr urls)
						    (funcall l (cdr urls))))))))
			     (funcall l ',geo-ip-urls)) t))
		 (lambda (it)
		   (pcase-let* ((`(,item . ,entry) it)
				(`(,lat . ,lon) (geo-ip--extract-latl entry item)))
		     (when (and lat lon
				(numberp lat)
				(numberp lon))
		       (setq geo-ip--last-callback-success-p t)
		       (let ((loc (geo-location lat lon (round (float-time)) nil)))
			 (when (not (equal geo-ip--last-location loc))
			   (setq geo-ip--last-location loc)
			   (funcall callback loc)))))))))

(defun geo-ip--timer-callback ()
  "Callback to be run from the geo-ip refresh timer."
  (geo-ip--async-retrieve-ip (lambda (location)
			       (run-hook-with-args 'geo-ip--changed-hook location))))

(defun geo-ip--last-ip-invalid-p ()
  "Whether or not the last call was unsuccessful."
  (not geo-ip--last-callback-success-p))

(defun geo-ip--subscribe (fn)
  "Subscribe FN to geo-ip events."
  (when geo-ip--last-location
    (funcall fn geo-ip--last-location))
  (add-hook 'geo-ip--changed-hook fn))

(run-with-timer 0 (* 60 2000) #'geo-ip--timer-callback)

(geo-enable-backend #'geo-ip--subscribe
		    #'geo-ip--last-ip-invalid-p 2)

(provide 'geo-ip)
;;; geo-ip.el ends here
