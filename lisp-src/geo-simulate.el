;;; geo-simulate.el --- Simulate geo data  -*- lexical-binding: t -*-

;; Copyright (C) 2020 by Zajcev Evgeny.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Fri Dec 18 10:26:25 2020

;; This file is NOT part of GNU Emacs.

;; geo-simulate.el is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; geo-simulate.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with geo-simulate.el. If not, see
;; <https://www.gnu.org/licenses/>.

;;; Commentary:

;; geo backend to simulate location data.  Pretend you are moving
;; along a given points in a given time.  Start simulating location
;; data by running `geo-simulate' function.

;;; Code:

(require 'geo)

(defvar geo-simulate-update-period 5.0
  "Update location in every this seconds.")

(defvar geo-simulate--points nil
  "List of points to simulate moving across.")

(defvar geo-simulate--current-point nil
  "Last bypassed point in `geo-simulate--points'.")

(defvar geo-simulate--speed nil
  "Calculated simulation speed.")

(defvar geo-simulate--timer nil)

(defvar geo-simulate--loc nil
  "Last simulated location.")

(defvar geo-simulate-location-changed-hook nil
  "Hook called when simulated location changes.")

(defun geo-simulate-distance (loc1 loc2)
  "Return distance in meters between locations LOC1 and LOC2."
  (let* ((lat1 (geo-location-lat loc1))
         (lat2 (geo-location-lat loc2))
         (lon1 (geo-location-lon loc1))
         (lon2 (geo-location-lon loc2))
         (lat-d (* 111111 (- lat1 lat2)))
         (lon-d (* 111111 (cos (degrees-to-radians (/ (+ lat1 lat2) 2)))
                   (- lon2 lon1))))
    (round (sqrt (+ (* lat-d lat-d) (* lon-d lon-d))))))

(defun geo-simulate--timer-func ()
  "Function called to update simulated location."
  (if (not geo-simulate--current-point)
      ;; Initial call, start from first point
      (setq geo-simulate--current-point (car geo-simulate--points)
            geo-simulate--loc geo-simulate--current-point)

    ;; Point moved
    (let ((need-distance (* geo-simulate--speed
                            geo-simulate-update-period))
          (next-points (cdr (memq geo-simulate--current-point
                                  geo-simulate--points)))
          next-point-distance)
      ;; Skip all points closer then `need-distance'
      (while (and next-points
                  (< (setq next-point-distance
                           (geo-simulate-distance
                            geo-simulate--loc (car next-points)))
                     need-distance))
        (setq need-distance (- need-distance next-point-distance))
        (setq geo-simulate--current-point (car next-points))
        (setq geo-simulate--loc geo-simulate--current-point)
        (setq next-points (cdr next-points)))

      (if (not next-points)
          (progn
            ;; NOTE: All points are traversed, done processing
            (cl-assert (eq geo-simulate--current-point
                           (car (last geo-simulate--points))))
            (setq geo-simulate--loc geo-simulate--current-point)
            (cancel-timer geo-simulate--timer)
            (setq geo-simulate--timer nil))

        ;; Calculate next location
        (cl-assert next-points)
        (cl-assert next-point-distance)
        (cl-assert (<= need-distance next-point-distance))
        (let ((d-ratio (/ need-distance next-point-distance))
              (lat (geo-location-lat geo-simulate--loc))
              (lon (geo-location-lon geo-simulate--loc))
              (n-lat (geo-location-lat (car next-points)))
              (n-lon (geo-location-lon (car next-points))))
          (setq geo-simulate--loc
                (list (cons 'lat (+ lat (* (- n-lat lat) d-ratio)))
                      (cons 'lon (+ lon (* (- n-lon lon) d-ratio))))))
        )))

  (cl-assert geo-simulate--loc)
  (run-hook-with-args 'geo-simulate-location-changed-hook geo-simulate--loc))

(defun geo-simulate (points seconds)
  "Simulate moving across POINTS in SECONDS time.
At least two points must be in POINTS.
Each point is an alist in form:
 ((lat . <LATITUDE>) (lon . <LONGITUDE>))."
  (cl-assert (> (length points) 1))
  (cl-assert (> seconds 0))

  (when geo-simulate--timer
    (cancel-timer geo-simulate--timer)
    (setq geo-simulate--timer nil))

  (setq geo-simulate--points points)
  (let ((distance
         (cdr (cl-reduce (lambda (accum pnt)
                           (cons pnt
                                 (+ (cdr accum)
                                    (geo-simulate-distance (car accum) pnt))))
                         (cdr geo-simulate--points)
                         :initial-value (cons (car geo-simulate--points) 0)))))
    (setq geo-simulate--speed (/ distance seconds)))

  (setq geo-simulate--current-point nil)
  (setq geo-simulate--timer
        (run-with-timer 0 geo-simulate-update-period
                        #'geo-simulate--timer-func))
  )


;; Backend functions
(defun geo-simulate--subscribe (fn)
  "Register the Geo backend FN to receive location callbacks."
  (add-hook 'geo-simulate-location-changed-hook fn))

(defun geo-simulate-pause ()
  "Pause geo data simulation."
  (when geo-simulate--timer
    (cancel-timer geo-simulate--timer)))

(defun geo-simulate-resume ()
  "Resume geo data simulation."
  (when geo-simulate--timer
    (setq geo-simulate--timer
          (run-with-timer 0 geo-simulate-update-period
                          #'geo-simulate--timer-func))))

(geo-enable-backend #'geo-simulate--subscribe #'ignore 1
                    #'geo-simulate-pause
                    #'geo-simulate-resume)

(provide 'geo-simulate)

;;; geo-simulate.el ends here
