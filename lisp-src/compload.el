;;; compload.el --- File to be loaded when compiling geo.el

;; Copyright (c) 2020 Would (oldosfan)
;; See the file COPYING in the root directory for
;; copying information.

;;; Commentary:

;; When compiling Emacs Lisp, required packages such as
;; async.el are not loaded.  Loading this file tries to
;; load the required packages, and also adds the current
;; directory to the load-path, in order to load in-tree
;; dependencies.

;;; Code:

(package-initialize)
(add-to-list 'load-path default-directory)
(load "bytecomp")

(provide 'compload)

;;; compload.el ends here
