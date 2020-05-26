;;; geo-nm.el --- NetworkManager and wifi geolocation backend for geo.el  -*- lexical-binding: t; -*-

;; Copyright (c) 2020 Would (oldosfan).

;; This file is not part of GNU Emacs.

;; geo-nm.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; geo-nm.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with geo-nm.el. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; geo-nm is a Geo backend using NetworkManager and the Mozilla
;; geolocation API.

;;; Code:

(require 'dbus)
(require 'geo)
(require 'cl-lib)
(require 'json)
(require 'async)

(defvar geo-nm-moz-key "geoclue"
  "The API key to be used for requests.")

(defvar geo-nm-last-result nil
  "The last response retrieved.")

(defvar geo-nm-changed-hook nil
  "A list of functions to be called with `geo-nm-last-result' when it changes.")

(defvar geo-nm--last-call-successful-p nil
  "Whether or not the last API request was successful.")

(defvar geo-nm--last-call nil
  "The last call to async.el.")

(defvar geo-nm-delay 25.0
  "The minimum time (in seconds) between geo-nm refreshes.")

(defvar geo-nm--last-call-time nil
  "The time of the last geo-nm refresh.")

(defun geo-nm--nm-available-p ()
  "Return non-nil if NetworkManager is available."
  (ignore-errors
    (dbus-call-method :system "org.freedesktop.NetworkManager"
		      "/org/freedesktop/NetworkManager"
		      "org.freedesktop.NetworkManager"
		      "state")))

(defun geo-nm--ap-callback (&rest _)
  "Callback to be run on AP list change."
  (geo-nm--timer-callback))

(defun geo-nm--device-added-cb (device)
  "Callback that will be called when a NetworkManager device has been added.
DEVICE should be an object path leading to DEVICE."
  (when (eq (geo-nm--device-type device) 2)
    (dbus-register-signal :system "org.freedesktop.NetworkManager"
			  device "org.freedesktop.NetworkManager.Device.Wireless"
			  "AccessPointAdded" #'geo-nm--ap-callback)
    (dbus-register-signal :system "org.freedesktop.NetworkManager"
			  device "org.freedesktop.NetworkManager.Device.Wireless"
			  "AccessPointRemoved" #'geo-nm--ap-callback)))

(defun geo-nm--ap-properties (ap)
  "Return the properties of the access point AP, as an alist."
  (mapcar (lambda (i)
	    (cons (intern (downcase (car i)))
		  (if (not (equal (downcase (car i)) "ssid")) (cdr i)
		    (apply #'string (cdr i)))))
	  (dbus-get-all-properties :system "org.freedesktop.NetworkManager"
				   ap "org.freedesktop.NetworkManager.AccessPoint")))

(defun geo-nm--device-type (dev)
  "Return the device type from DEV."
  (ignore-errors
    (dbus-get-property :system "org.freedesktop.NetworkManager"
		       dev "org.freedesktop.NetworkManager.Device"
		       "DeviceType")))

(defun geo-nm--get-devices ()
  "Return a list of valid devices from NetworkManager."
  (let ((devl (dbus-call-method :system
				"org.freedesktop.NetworkManager"
				"/org/freedesktop/NetworkManager"
				"org.freedesktop.NetworkManager"
				"GetDevices")))
    (cl-remove-if-not (lambda (l)
			(eq (geo-nm--device-type l) 2)) devl)))

(defun geo-nm--ap-ssid (ap)
  "Return the SSID from the access point alist AP."
  (cdr (assq 'ssid ap)))

(defun geo-nm--ap-hwaddr (ap)
  "Return the MAC address from the access point alist AP."
  (cdr (assq 'hwaddress ap)))

(defun geo-nm--wireless-device-aps (device)
  "Return a list of access points from DEVICE."
  (dbus-call-method :system
		    "org.freedesktop.NetworkManager"
		    device
		    "org.freedesktop.NetworkManager.Device.Wireless"
		    "GetAccessPoints"))

(defun geo-nm--get-aps ()
  "Return a list of access points from NetworkManager."
  (let ((devices (geo-nm--get-devices)))
    (remove-if #'geo-nm--nomap-p
	       (mapcar #'geo-nm--ap-properties
		       (cl-reduce #'append
				  (mapcar #'geo-nm--wireless-device-aps devices))))))

(defun geo-nm--get-ssids ()
  "Return a list of SSIDs from NetworkManager."
  (mapcar #'geo-nm--ap-ssid (geo-nm--get-aps)))

(defun geo-nm--get-hwaddrs ()
  "Return a list of hardware addresses from NetworkManager."
  (mapcar #'geo-nm--ap-hwaddr (geo-nm--get-aps)))

(defun geo-nm--moz-callback (data)
  "Callback for `geo-nm--async-fetch-json'.
DATA should be the returned JSON data."
  (setq geo-nm--last-call-time (float-time))
  (ignore-errors
    (let ((l geo-nm-last-result))
      (setq geo-nm-last-result
	    (list (assq 'lat (cdar data))
		  (cons 'lon (cdr (assq 'lng (cdar data))))
		  (cons 'dt (round (float-time)))))
      (setq geo-nm--last-call-successful-p t)
      (unless (equal (subseq l 0 2)
		     (subseq geo-nm-last-result 0 2))
	(run-hook-with-args 'geo-nm-changed-hook geo-nm-last-result)))))

(defun geo-nm--strength-to-dbm (strength)
  "Convert STRENGTH, an integer between 0 and 100 to dBm."
  (- (/ strength 2) 100))

(defun geo-nm--ap-dbm (ap)
  "Return the dBm value of the access point AP."
  (geo-nm--strength-to-dbm (cdr (assq 'strength ap))))

(defun geo-nm--ap-frequency (ap)
  "Return the frequency in use by the access point AP."
  (cdr (assq 'frequency ap)))

(defun geo-nm--json-data (ap)
  "Encode the access point AP into JSON data for the MLS API."
  `((macAddress . ,(geo-nm--ap-hwaddr ap))
    (frequency . ,(geo-nm--ap-frequency ap))
    (signalStrength . ,(geo-nm--ap-dbm ap))))

(defun geo-nm--nomap-p (ap)
  "Return whether AP should be ignored in requests."
  (and (stringp (geo-nm--ap-ssid ap))
       (string-suffix-p "_nomap" (geo-nm--ap-ssid ap))))

(defun geo-nm--async-fetch-json (cb)
  "Fetch the raw json data from Mozilla's GeoClue API asynchronously.
CB will be called with the data as a string."
  (when (geo-nm--nm-available-p)
    (ignore-errors
      (setq geo-nm--last-call
	    (async-start
	     `(lambda ()
		(require 'url)
		(require 'json)
		(let ((url-http-data (json-encode
				      ,(list 'quote
					     `((wifiAccessPoints . ,(mapcar #'geo-nm--json-data
									    (geo-nm--get-aps)))))))
		      (url-http-method "POST"))
		  (with-current-buffer (url-retrieve-synchronously
					(format "https://location.services.mozilla.com/v1/geolocate?key=%s"
						,geo-nm-moz-key))
		    (goto-char (point-min))
		    (search-forward "{")
		    (previous-line)
		    (delete-region (point-min) (point))
		    (json-read)))) cb)))))

(defun geo-nm--subscribe (cb)
  "Subscribe CB as a change listener."
  (when geo-nm-last-result
    (funcall cb geo-nm-last-result))
  (add-hook 'geo-nm-changed-hook cb))

(defun geo-nm--data-invalid-p ()
  "Return whether the current data should be considered invalid."
  (not geo-nm--last-call-successful-p))

(defun geo-nm--timer-callback ()
  "Timer callback for the geo-nm refresh timer."
  (setq geo-nm--last-call-successful-p nil)
  (unless (and geo-nm--last-call
	       (process-live-p geo-nm--last-call)
	       (not (and geo-nm--last-call-time
			 (< (- geo-nm--last-call-time
			       (float-time))
			    geo-nm-delay))))
    (geo-nm--async-fetch-json #'geo-nm--moz-callback)))

(run-with-timer 0 nil #'geo-nm--timer-callback)

(geo-enable-backend #'geo-nm--subscribe
		    #'geo-nm--data-invalid-p 2)

(dbus-register-signal :system
		      "org.freedesktop.NetworkManager"
		      "/org/freedesktop/NetworkManager"
		      "org.freedesktop.NetworkManager"
		      "DeviceAdded"
		      #'geo-nm--device-added-cb)

(mapc #'geo-nm--device-added-cb (geo-nm--get-devices))

(provide 'geo-nm)

;;; geo-nm.el ends here
