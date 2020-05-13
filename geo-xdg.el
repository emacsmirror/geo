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

(defvar geo-xdg--client nil
  "The current XDG geo client.")

(defvar geo-xdg--client-already-connected nil
  "Whether or not the LocationUpdated has already been attached.")

(defvar geo-xdg--last-location nil
  "The last registered location.")

(defvar geo-xdg--things-to-unregister nil
  "A list of items to be unregistered.")

(defvar geo-xdg-changed-hooks nil
  "A list of functions to be called with the location data when it is updated.")

(defvar geo-xdg-cache-function #'geo-xdg-get-cache
  "A function that returns the cached location.")

(defvar geo-xdg-save-cache-function #'geo-xdg-save-cache
  "A function that saves the cached value.
It should take one argument, the value to be saved.")

(cl-deftype geo-xdg--location ()
  '(satisfies geo-xdg--location-p))

(defun geo-xdg--available-p ()
  "Return non-nil if GeoClue is available."
  (dbus-ping :system "org.freedesktop.GeoClue2" 100))

(defun geo--create-xdg-client ()
  "Create a GeoClue client."
  (if geo-xdg--client geo-xdg--client
    (dbus-call-method
     :system "org.freedesktop.GeoClue2"
     "/org/freedesktop/GeoClue2/Manager"
     "org.freedesktop.GeoClue2.Manager"
     "CreateClient")))

(defun geo-xdg-save-cache (cache)
  "Set CACHE as the cached location."
  (ignore-errors
    (save-window-excursion
      (with-current-buffer (find-file (concat user-emacs-directory
					      "geo-xdg-cache.el"))
	(erase-buffer)
	(print cache (current-buffer))
	(save-buffer)
	(kill-buffer)))))

(defun geo-xdg-get-cache ()
  "Retrieve the cached location."
  (or
   (save-window-excursion
     (when (file-exists-p (concat user-emacs-directory
				  "geo-xdg-cache.el"))
       (with-current-buffer (find-file (concat user-emacs-directory
					       "geo-xdg-cache.el"))
	 (set-window-point nil (point-min))
	 (read (current-buffer)))))
   (and (boundp 'calendar-latitude)
	(boundp 'calendar-longitude)
	`((lat . ,calendar-latitude)
	  (lon . ,calendar-longitude)
	  (alt . 0.0)
	  (speed . 0.0)
	  (description . "")))))

(defun geo-xdg--maybe-setup ()
  "Set up GeoClue related interfaces if necessary."
  (when (and (not geo-xdg--client)
	     (geo-xdg--available-p))
    (setq geo-xdg--client (geo--create-xdg-client))))

(defun geo-xdg--location-p (loc)
  "Return non-nil if LOC should be considered a `org.freedesktop.GeoClue2.Location'."
  (and (stringp loc)
       (string-prefix-p (concat geo-xdg--client "/Location") loc)))

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

(defun geo-xdg--location-data (loc)
  "Return the data stored inside LOC.
The returned data will be stored in the following format:
  ((lat . latitude)
   (lon . longitude)
   (alt . altitude)
   (speed . speed)
   (description . description))"
  (cl-check-type loc geo-xdg--location)
  `((lat . ,(dbus-get-property :system "org.freedesktop.GeoClue2"
			       loc "org.freedesktop.GeoClue2.Location"
			       "Latitude"))
    (lon . ,(dbus-get-property :system "org.freedesktop.GeoClue2"
			       loc "org.freedesktop.GeoClue2.Location"
			       "Longitude"))
    (alt . ,(dbus-get-property :system "org.freedesktop.GeoClue2"
			       loc "org.freedesktop.GeoClue2.Location"
			       "Altitude"))
    (speed . ,(dbus-get-property :system "org.freedesktop.GeoClue2"
				 loc "org.freedesktop.GeoClue2.Location"
				 "Speed"))
    (description . ,(dbus-get-property :system "org.freedesktop.GeoClue2"
				       loc "org.freedesktop.GeoClue2.Location"
				       "Description"))))

(defun geo-xdg--dbus-callback (_ new)
  "Callback for GeoClue2's LocationUpdated signal.
NEW should be the new location as an `org.freedesktop.GeoClue2.Location'"
  (ignore-errors
    (setq geo-xdg--last-location (geo-xdg--location-data new)))
  (ignore-errors
    (funcall geo-xdg-save-cache-function geo-xdg--last-location))
  (ignore-errors
    (run-hook-with-args 'geo-xdg-changed-hooks geo-xdg--last-location)))

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
  (dbus-call-method :system "org.freedesktop.GeoClue2"
		    geo-xdg--client
		    "org.freedesktop.GeoClue2.Client"
		    "Stop"))

(defun geo-xdg--destroy-client ()
  "Destroy and nil out `geo-xdg--client'."
  (when geo-xdg--client
    (setq geo-xdg--client-already-connected nil)
    (dbus-call-method
     :system "org.freedesktop.GeoClue2"
     "/org/freedesktop/GeoClue2/Manager"
     "org.freedesktop.GeoClue2.Manager"
     "DeleteClient"
     :object-path geo-xdg--client)
    (setq geo-xdg--client nil)
    (mapc #'dbus-unregister-object geo-xdg--things-to-unregister)))

(defun geo-xdg--maybe-setup-timer-cb ()
  "Periodically check if GeoClue is available, and set up the client if it is."
  (when (not geo-xdg--client)
    (when (geo-xdg--available-p)
      (geo-xdg--maybe-setup)
      (geo-xdg--register-signals))))

(defun geo-xdg--restore-from-cached-value ()
  "Restore the current location from the cached value."
  (let ((c (funcall geo-xdg-cache-function)))
    (when c
      (setq geo-xdg--last-location c)
      (ignore-errors
	(run-hook-with-args 'geo-xdg-changed-hooks c)))))

(geo-xdg--restore-from-cached-value)

(run-with-timer 0 60 #'geo-xdg--maybe-setup-timer-cb)

(provide 'geo-xdg)
;;; geo-xdg.el ends here
