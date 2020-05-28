;;; geocode.el --- reverse geocoding services for GNU Emacs

;; geocode-emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; geocode-emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with geocode-emacs. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; geocode-emacs provides reverse geocoding services for GNU Emacs
;; via the geocode-glib library.

;;; Code:

(eval-when-compile
  (add-to-list 'load-path ".")
  (package-initialize))

(require 'geocode-emacs)
(require 'async)
(require 'cl-lib)

(defun geocode-async-resolve-lat-lon (lat lon accuracy cb)
  "Resolve the object at LAT, LON.
ACCURACY should be the accuracy of the location as a double
in metres.
CB will be called with the returned place object."
  (cl-check-type lat floatp)
  (cl-check-type lon floatp)
  (cl-check-type accuracy floatp)
  (async-start
   `(lambda ()
      (ignore-errors
	(resolve-geocode-place (make-geocode-location ,lat ,lon ,accuracy))))
   cb))

(provide 'geocode)

;;; geocode.el ends here
