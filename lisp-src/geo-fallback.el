;;; geo-fallback.el --- geo-fallback.el -- Fallback Geo backend  -*- lexical-binding: t; -*-

;; Copyright (c) 2020 Would (oldosfan).

;; This file is not part of GNU Emacs.

;; geo-fallback.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; geo-fallback.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with geo-fallback.el. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; A fallback Geo provider that accepts a fixed lat and lon.

;;; Code:

(require 'geo)

(defvar geo-fallback-lat 0.0
  "The fallback latitude for `geo-fallback'.")

(defvar geo-fallback-lon 0.0
  "The fallback longitude for `geo-fallback'.")

(defvar geo-fallback-registered-handlers nil
  "A list of handlers for geo-fallback.")

(defvar geo-fallback-paused-p nil
  "Whether or not geo-fallback is paused.")

(defun geo-fallback-notify-changed ()
  "Run geo-fallback handlers."
  (unless geo-fallback-paused-p
    (run-hook-with-args 'geo-fallback-registered-handlers
			(geo-location geo-fallback-lat
				      geo-fallback-lon
				      (round (float-time)) nil))))

(defun geo-fallback--subscribe (fn)
  "Subscribe FN to `geo-fallback-registered-handlers'."
  (add-hook 'geo-fallback-registered-handlers fn)
  (geo-fallback-notify-changed))

(defun geo-fallback--outdated-p ()
  "Return t."
  t)

(defun geo-fallback--pause ()
  "Pause geo-fallback."
  (setq geo-fallback-paused-p t))

(defun geo-fallback--resume ()
  "Resume geo-fallback."
  (setq geo-fallback-paused-p nil)
  (geo-fallback-notify-changed))

(geo-enable-backend #'geo-fallback--subscribe
		    #'geo-fallback--outdated-p 0
		    #'geo-fallback--pause
		    #'geo-fallback--resume)

(add-variable-watcher 'geo-fallback-lat #'geo-fallback-notify-changed)
(add-variable-watcher 'geo-fallback-lon #'geo-fallback-notify-changed)

(provide 'geo-fallback)
;;; geo-fallback.el ends here
