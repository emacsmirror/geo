;;; geo-xdg.el --- Geolocation services for GNU Emacs -*- lexical-binding: t; -*-

;; Copyright (c) 2020 Would (oldosfan).

;; This file is not part of GNU Emacs.

;; geo-xdg.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; geo-xdg.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with geo-xdg.el. If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;;; A small geolocation wrapper around the XDG's Geoclue API.

;;; Code:

(require 'dbus)
(require 'cl-lib)
(require 'geo)

(declare-function userlock--check-content-unchanged "userlock.el")
(declare-function geo-enable-backend "geo.el")

(defvar geo-xdg--client nil
  "The current XDG geo client.")

(defvar geo-xdg--client-already-connected nil
  "Whether or not the LocationUpdated has already been attached.")

(defvar geo-xdg--things-to-unregister nil
  "A list of items to be unregistered.")

(defvar geo-xdg-changed-hooks nil
  "A list of functions to be called with the location data when it is updated.")

(defvar geo-xdg-heading-changed-hook nil
  "A list of functions to be called with heading data on update.")

(defvar geo-xdg-last-location nil
  "The last registered location.")

(defvar geo-xdg-last-heading nil
  "The last registered heading.")

(defvar geo-xdg-cache-function #'geo-xdg-get-cache
  "A function that returns the cached location.")

(defvar geo-xdg-restored-from-cache t
  "Whether or not the last location was restored from cache.")

(defvar geo-xdg-save-cache-function #'geo-xdg-save-cache
  "A function that saves the cached value.
It should take one argument, the value to be saved.")

(defmacro geo-xdg--suppress-userlock (retval &rest body)
  "Ignore `yes-or-no-p' and always return RETVAL inside BODY."
  (declare (indent 1))
  `(let ((old-fn (symbol-function #'userlock--check-content-unchanged)))
     (fset 'userlock--check-content-unchanged (lambda (&rest _) ,retval))
     (unwind-protect (progn ,@body)
       (fset 'userlock--check-content-unchanged old-fn))))

(cl-deftype geo-xdg--location ()
  '(satisfies geo-xdg--location-p))

(defun geo-xdg--available-p ()
  "Return non-nil if GeoClue is available."
  (dbus-ping :system "org.freedesktop.GeoClue2" 100))

(defun geo-xdg--client-usable-p ()
  "Return non-nil if `geo-xdg--client' is a usable GeoClue client."
  (dbus-introspect-get-interface :system "org.freedesktop.GeoClue2"
				 geo-xdg--client "org.freedesktop.GeoClue2.Client"))

(defun geo--create-xdg-client ()
  "Create a GeoClue client."
  (if (and geo-xdg--client
	   (geo-xdg--client-usable-p))
      geo-xdg--client
    (dbus-call-method
     :system "org.freedesktop.GeoClue2"
     "/org/freedesktop/GeoClue2/Manager"
     "org.freedesktop.GeoClue2.Manager"
     "CreateClient")))

(defun geo-xdg-save-cache (cache)
  "Set CACHE as the cached location."
  (ignore-errors
    (save-window-excursion
      (geo-xdg--suppress-userlock t
	(with-current-buffer (find-file-noselect (concat user-emacs-directory
							 "geo-xdg-cache.el")
						 nil nil nil)
	  (erase-buffer)
	  (insert ";; -*- mode: lisp-data -*-\n")
	  (print cache (current-buffer))
	  (let ((save-silently t))
	    (save-buffer))
	  (kill-buffer))))))

(defun geo-xdg-get-cache ()
  "Retrieve the cached location."
  (save-window-excursion
    (when (file-exists-p (concat user-emacs-directory
				 "geo-xdg-cache.el"))
      (geo-xdg--suppress-userlock t
	(with-current-buffer (find-file-noselect (concat user-emacs-directory
							 "geo-xdg-cache.el")
						 nil nil nil)
	  (goto-char (point-min))
	  (prog1 (read (current-buffer))
	    (kill-buffer)))))))

(defun geo-xdg--maybe-setup ()
  "Set up GeoClue related interfaces if necessary."
  (when (or (and (not geo-xdg--client)
		 (geo-xdg--available-p))
	    (not (geo-xdg--client-usable-p)))
    (setq geo-xdg--things-to-unregister nil)
    (setq geo-xdg--client-already-connected nil)
    (setq geo-xdg--client (geo--create-xdg-client))))

(defun geo-xdg--location-p (loc)
  "Return non-nil if LOC should be considered a `org.freedesktop.GeoClue2.Location'."
  (and (stringp loc)
       (ignore-errors
	 (member "org.freedesktop.GeoClue2.Location"
		 (dbus-introspect-get-interface-names :system "org.freedesktop.GeoClue2"
						      loc)))))

(defun geo-xdg-location-lat-long (loc)
  "Return the latitude and longitude stored in LOC as a cons pair."
  (cons (cdr (assq 'lat loc))
	(cdr (assq 'lon loc))))

(defun geo-xdg-lat (loc)
  "Return the latitude stored inside LOC."
  (car (geo-xdg-location-lat-long loc)))

(defun geo-xdg-lon (loc)
  "Return the longitude stored inside LOC."
  (cdr (geo-xdg-location-lat-long loc)))

(defun geo-xdg--regularize-altitude (alt)
  "Regularize the altitude ALT.
If ALT is `-1.7976931348623157e+308', nil will be returned."
  (when (not (equal alt -1.7976931348623157e+308)) alt))

(defun geo-xdg--location-data (loc)
  "Return the data stored inside LOC.
The returned data will be stored in the following format:
  ((lat . latitude)
   (lon . longitude)
   (alt . altitude)
   (speed . speed)
   (accuracy . accuracy)
   (description . description)
   (dt . date-time))"
  (cl-check-type loc geo-xdg--location)
  `((lat . ,(dbus-get-property :system "org.freedesktop.GeoClue2"
			       loc "org.freedesktop.GeoClue2.Location"
			       "Latitude"))
    (lon . ,(dbus-get-property :system "org.freedesktop.GeoClue2"
			       loc "org.freedesktop.GeoClue2.Location"
			       "Longitude"))
    (alt . ,(geo-xdg--regularize-altitude (dbus-get-property :system "org.freedesktop.GeoClue2"
							     loc "org.freedesktop.GeoClue2.Location"
							     "Altitude")))
    (speed . ,(dbus-get-property :system "org.freedesktop.GeoClue2"
				 loc "org.freedesktop.GeoClue2.Location"
				 "Speed"))
    (accuracy . ,(dbus-get-property :system "org.freedesktop.GeoClue2"
				    loc "org.freedesktop.GeoClue2.Location"
				    "Accuracy"))
    (description . ,(dbus-get-property :system "org.freedesktop.GeoClue2"
				       loc "org.freedesktop.GeoClue2.Location"
				       "Description"))
    (dt . ,(car (dbus-get-property :system "org.freedesktop.GeoClue2"
				   loc "org.freedesktop.GeoClue2.Location"
				   "Timestamp")))))

(defun geo-xdg--location-heading (location)
  "Return the heading from LOCATION."
  (dbus-get-property :system "org.freedesktop.GeoClue2"
		     location "org.freedesktop.GeoClue2.Location"
		     "Heading"))

(defun geo-xdg--dbus-callback (_ new)
  "Callback for GeoClue2's LocationUpdated signal.
NEW should be the new location as an `org.freedesktop.GeoClue2.Location'"
  (let ((ld (geo-xdg--location-data new))
	(hd (geo-xdg--location-heading new)))
    (when (not (equal geo-xdg-last-location ld))
      (ignore-errors
	(setq geo-xdg-last-location ld))
      (setq geo-xdg-restored-from-cache nil)
      (ignore-errors
	(funcall geo-xdg-save-cache-function geo-xdg-last-location))
      (ignore-errors
	(run-hook-with-args 'geo-xdg-changed-hooks geo-xdg-last-location)))
    (when (not (equal geo-xdg-last-heading hd))
      (ignore-errors
	(setq geo-xdg-last-heading hd))
      (ignore-errors
	(run-hook-with-args 'geo-xdg-heading-changed-hook geo-xdg-last-heading)))))

(defun geo-xdg--register-signals ()
  "Register the LocationUpdated signal for the current GeoClue client."
  (geo-xdg--maybe-setup)
  (unless geo-xdg--client-already-connected
    (add-to-list 'geo-xdg--things-to-unregister
		 (dbus-set-property :system "org.freedesktop.GeoClue2"
				    geo-xdg--client
				    "org.freedesktop.GeoClue2.Client"
				    "DesktopId"
				    "Emacs"))
    (add-to-list 'geo-xdg--things-to-unregister
		 (dbus-register-signal :system "org.freedesktop.GeoClue2"
				       geo-xdg--client
				       "org.freedesktop.GeoClue2.Client"
				       "LocationUpdated"
				       #'geo-xdg--dbus-callback))
    (setq geo-xdg--client-already-connected t))
  (dbus-call-method :system "org.freedesktop.GeoClue2"
		    geo-xdg--client
		    "org.freedesktop.GeoClue2.Client"
		    "Start"))

(defun geo-xdg--unregister-signals ()
  "Stop recieving LocationUpdated signals."
  (geo-xdg--maybe-setup)
  (dbus-call-method :system "org.freedesktop.GeoClue2"
		    geo-xdg--client
		    "org.freedesktop.GeoClue2.Client"
		    "Stop"))

(defun geo-xdg--destroy-client ()
  "Destroy and nil out `geo-xdg--client'."
  (when (and geo-xdg--client
	     (geo-xdg--client-usable-p))
    (setq geo-xdg--client-already-connected nil)
    (dbus-call-method
     :system "org.freedesktop.GeoClue2"
     "/org/freedesktop/GeoClue2/Manager"
     "org.freedesktop.GeoClue2.Manager"
     "DeleteClient"
     :object-path geo-xdg--client)
    (setq geo-xdg--client nil)
    (mapc #'dbus-unregister-object geo-xdg--things-to-unregister))
  (when (and geo-xdg--client
	     (not (geo-xdg--client-usable-p)))
    (setq geo-xdg--client nil)))

(defun geo-xdg--maybe-setup-timer-cb ()
  "Periodically check if GeoClue is available, and set up the client if it is."
  (when (and (not geo-xdg--client)
	     (geo-xdg--available-p))
    (geo-xdg--maybe-setup)
    (geo-xdg--register-signals)))

(defun geo-xdg--restore-from-cached-value ()
  "Restore the current location from the cached value."
  (let ((c (funcall geo-xdg-cache-function)))
    (setq geo-xdg-last-location c)
    (when c
      (ignore-errors
	(run-hook-with-args 'geo-xdg-changed-hooks c)))))

(defun geo-xdg-pause ()
  "Stop GeoClue update signals."
  (interactive)
  (geo-xdg--maybe-setup)
  (geo-xdg--unregister-signals))

(defun geo-xdg-resume ()
  "Resume GeoClue update signals."
  (interactive)
  (geo-xdg--maybe-setup)
  (geo-xdg--register-signals))

(defun geo-xdg-focus-changed ()
  "Convenience function to be added to `after-focus-change-function'."
  (if (not (cl-reduce (lambda (z y)
			(and z (frame-focus-state y)))
		      (frame-list)))
      (geo-xdg-pause)
    (geo-xdg-resume)))

(geo-xdg--restore-from-cached-value)

(run-with-timer 0 60 #'geo-xdg--maybe-setup-timer-cb)

(defun geo-xdg--data-outdated-p ()
  "Return whether the current data was restored from cache."
  geo-xdg-restored-from-cache
  (> (- (float-time) (cdr (assq 'dt geo-xdg-last-location))) 100000))

(defun geo-xdg--geo-register (fn)
  "Register the Geo backend FN to receive location callbacks."
  (add-hook 'geo-xdg-changed-hooks fn)
  (run-hook-with-args 'geo-xdg-changed-hooks geo-xdg-last-location))

(defun geo-xdg--geo-register-for-heading (fn)
  "Register the Geo backend FN to receive heading callbacks."
  (add-hook 'geo-xdg-heading-changed-hook fn)
  (run-hook-with-args 'geo-xdg-heading-changed-hook geo-xdg-last-heading))

(eval-after-load 'geo
  '(geo-enable-backend #'geo-xdg--geo-register
		       #'geo-xdg--data-outdated-p 3
		       #'geo-xdg-pause
		       #'geo-xdg-resume
		       #'geo-xdg--geo-register-for-heading))

(provide 'geo-xdg)
;;; geo-xdg.el ends here
