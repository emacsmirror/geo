;;; geo-xdg-solar.el --- Calendar location services for GNU Emacs, using geo-xdg.

;; Copyright (c) 2020 Would (oldosfan).

;; This file is not part of GNU Emacs.

;; geo-xdg-solar.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; geo-xdg-solar.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with geo-xdg-solar.el. If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;;; Provides GeoClue information to calendar.el.

;;; Code:

(require 'geo-xdg)
(require 'solar)

(defun geo-xdg-circadian--on-changed (geo)
  "Set circadian geo values according to GEO."
  (when geo
    (setq calendar-latitude (geo-xdg-lat geo))
    (setq calendar-longitude (geo-xdg-lon geo))))

(defun geo-xdg-circadian--cache-advice (other &rest args)
  "Advice for OTHER with ARGS."
  (or (apply other args)
      (and calendar-latitude
	   calendar-longitude
	   `((lat . ,calendar-latitude)
	     (lon . ,calendar-longitude)
	     (alt . 0.0)
	     (speed . 0.0)
	     (description . "")))))

(advice-add 'geo-xdg-get-cache :around #'geo-xdg-circadian--cache-advice)

(add-hook 'geo-xdg-changed-hooks #'geo-xdg-circadian--on-changed)

(geo-xdg-circadian--on-changed geo-xdg-last-location)
(geo-xdg--restore-from-cached-value)

(provide 'geo-xdg-solar)
;;; geo-xdg-solar.el ends here
