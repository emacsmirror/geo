;;; geo-solar.el --- Calendar location services for GNU Emacs, using geo-solar.

;; Copyright (c) 2020 Would (oldosfan).

;; This file is not part of GNU Emacs.

;; geo-solar.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; geo-solar.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with geo-solar.el. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Provides geo.el to solar.

;;; Code:

(require 'geo)
(require 'solar)

(defun geo-solar--on-location-change (loc)
  "Called when the current solar location has been changed to LOC."
  (when loc
    (setq calendar-latitude (geo-location-lat loc))
    (setq calendar-longitude (geo-location-lon loc))))

(defun geo-solar--xdg-cache-advice (other &rest args)
  "Advice for OTHER with ARGS."
  (or (apply other args)
      (when (and calendar-latitude
		 calendar-longitude)
	`((lat . ,calendar-latitude)
	  (lon . ,calendar-longitude)
	  (alt . 0.0)
	  (speed . 0.0)
	  (description . "")))))

(eval-after-load "geo-xdg"
  (lambda ()
    (advice-add 'geo-xdg-get-cache :around #'geo-solar--xdg-cache-advice)
    (geo-xdg--restore-from-cached-value)))

(add-hook 'geo-data-changed-hook #'geo-solar--on-location-change)
(geo-solar--on-location-change (geo-last-location))
(provide 'geo-solar)
;;; geo-solar.el ends here
