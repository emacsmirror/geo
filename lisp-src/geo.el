;;; geo.el --- Generic geolocation backend for GNU Emacs.  -*- lexical-binding: t; -*-

;; Copyright (c) 2020 Would (oldosfan).

;; This file is not part of GNU Emacs.

;; geo.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; geo.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with geo.el. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; geo.el is a generic location framework for GNU Emacs.

;;; Code:

(require 'cl-lib)

(defvar geo--enabled-backends nil
  "Enabled geo.el backends.")

(defvar geo--backend-slots nil
  "Slots for various geo.el backends.")

(defvar geo-data-changed-hook nil
  "Hook to be run with a single argument LOCATION when the location is changed.")

(defvar geo--paused-p nil
  "Whether or not geo.el updates are paused.")

(defvar geo--heading-backend-slots nil
  "Slots for various geo.el heading backends.")

(defvar geo-heading-changed-hook nil
  "Hook to be run with a single argument HEADING when the HEADING is changed.")

(cl-deftype geo--backend ()
  '(satisfies geo--backend-p))

(cl-deftype geo-location ()
  '(satisfies listp))

(defun geo--backend-p (backend)
  "Return non-nil if BACKEND is a Geo backend."
  (and (fixnump (car-safe backend))
       (functionp (ignore-errors (cadr backend)))
       (ignore-errors (eq (car (func-arity (cadr backend))) 1))
       (ignore-errors (functionp (caddr backend)))
       (ignore-errors (eq (car (func-arity (caddr backend))) 0))
       (or (null (cadddr backend))
	   (ignore-errors (eq (car (func-arity (cadddr backend))) 0)))
       (or (null (car (cddddr backend)))
	   (ignore-errors (eq (car (func-arity (car (cddddr backend)))) 0)))
       (or (null (car (cdr (cddddr backend))))
	   (ignore-errors (eq (car (func-arity (car (cdr (cddddr backend))))) 1)))))

(defun geo--update-handler (location backend)
  "Update handler for a geo backend.
LOCATION should be a `geo--location', or a heading.
BACKEND should be the geo.el backend."
  (when location
    (if (consp location)
	(progn
	  (let ((ll (geo-last-location)))
	    (when (and (not (equal ll location))
		       (not geo--paused-p))
	      (setf (alist-get backend geo--backend-slots nil nil #'equal) location)
	      (run-hook-with-args 'geo-data-changed-hook (geo--sort-slots)))))
      (let ((lh (geo-last-heading)))
	(when (and (not (equal lh location))
		   (not geo--paused-p))
	  (setf (alist-get backend geo--heading-backend-slots nil nil #'equal) location)
	  (run-hook-with-args 'geo-heading-changed-hook lh))))))

(defun geo--backend-register-function (backend fn)
  "Register a single function FN to be called when BACKEND's location is changed."
  (cl-check-type backend geo--backend)
  (funcall (cadr backend) fn)
  (when (car (cdr (cddddr backend)))
    (funcall (car (cdr (cddddr backend))) fn)))

(defun geo--backend-priority (backend)
  "Return BACKEND's priority.
The higher it is, the more important BACKEND is."
  (cl-check-type backend geo--backend)
  (car backend))

(defun geo--backend-info-outdated-p (backend)
  "Return whether the information in BACKEND is outdated."
  (cl-check-type backend geo--backend)
  (funcall (caddr backend)))

(defun geo--backend-pause-updates (backend)
  "Pause location update events for BACKEND."
  (cl-check-type backend geo--backend)
  (when (cadddr backend)
    (funcall (cadddr backend))))

(defun geo--backend-resume-updates (backend)
  "Resume location update events for BACKEND."
  (cl-check-type backend geo--backend)
  (when (car (cddddr backend))
    (funcall (car (cddddr backend)))))

(defun geo--sort-slots ()
  "Return the best entry in `geo--backend-slots'."
  (let* ((good-slots (cl-remove-if (lambda (i)
				     (geo--backend-info-outdated-p (car i)))
				   geo--backend-slots))
	 (bad-slots (cl-remove-if-not (lambda (i)
					(not (memq i good-slots)))
				      geo--backend-slots)))
    (let ((good-entry (car-safe (mapcar #'cdr (cl-sort good-slots (lambda (x i)
								    (if (not (eq (caar x) (caar i)))
									(> (caar x) (caar i))
								      (> (cdr (assq 'dt (cdr x)))
									 (cdr (assq 'dt (cdr i))))))))))
	  (bad-entry (car-safe (mapcar #'cdr (cl-sort bad-slots (lambda (x i)
								  (if (not (eq (caar x) (caar i)))
									(> (caar x) (caar i))
								      (> (cdr (assq 'dt (cdr x)))
									 (cdr (assq 'dt (cdr i)))))))))))
      (or good-entry bad-entry))))

(defun geo--sort-heading-slots ()
  "Return the best entry in `geo--heading-backend-slots'."
  (car-safe (mapcar #'cdr (cl-sort geo--heading-backend-slots (lambda (x i)
								(> (caar x) (caar i)))))))

(defun geo-enable-backend (subscribe-function outdated-p-function priority &optional
					      stop-function start-function heading-function)
  "Enable a backend with SUBSCRIBE-FUNCTION, OUTDATED-P-FUNCTION, and PRIORITY.
STOP-FUNCTION should be a function that pauses location updates.
START-FUNCTION should be a function that resumes location
updates.  HEADING-FUNCTION should be a function that accepts a
single argument FN, to be called when the heading is changed with
the heading as the sole argument."
  (let ((backend (list priority subscribe-function outdated-p-function
		       stop-function start-function heading-function)))
    (cl-check-type backend geo--backend)
    (unless (member geo--enabled-backends backend)
      (geo--backend-register-function backend (lambda (l)
						(geo--update-handler l backend)))
      (setq geo--enabled-backends (cons backend geo--enabled-backends)))))

(defun geo-location (lat lon timestamp rest)
  "Create a location with the latitude LAT, the longitude LON, and TIMESTAMP.
Additional data can be stored inside REST."
  (append `((lat . ,lat)
	    (lon . ,lon)
	    (dt . ,timestamp)) rest))

(defun geo-location-lat (loc)
  "Return LOC's latitude."
  (cdr (assq 'lat loc)))

(defun geo-location-lon (loc)
  "Return LOC's longitude."
  (cdr (assq 'lon loc)))

(defun geo-location-altitude (loc)
  "Return LOC's altitude, if it exists."
  (cdr-safe (assq 'alt (cdr loc))))

(defun geo-last-location ()
  "Return the last known location from geo.el."
  (geo--sort-slots))

(defun geo-last-heading ()
  "Return the last known heading from geo.el."
  (geo--sort-heading-slots))

(defun geo-pause ()
  "Pause location refreshes from geo.el."
  (interactive)
  (mapc #'geo--backend-pause-updates geo--enabled-backends)
  (setq geo--paused-p t))

(defun geo-resume ()
  "Resume location refreshes from geo.el."
  (interactive)
  (mapc #'geo--backend-resume-updates geo--enabled-backends)
  (setq geo--paused-p nil))

(defsubst geo-current-lat ()
  "Return the current latitude."
  (let ((loc (geo--sort-slots)))
    (when loc
      (geo-location-lat loc))))

(defsubst geo-current-lon ()
  "Return the current longitude."
  (let ((loc (geo--sort-slots)))
    (when loc
      (geo-location-lon loc))))

(provide 'geo)
;;; geo.el ends here
