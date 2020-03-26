(in-package #:cl-user)
(defpackage #:grains
  (:use #:cl)
  (:export :square :total))
(in-package #:grains)

(defvar max-n 64)

(defun square (n)
  (expt 2 (- n 1)))

(defun total ()
  (- (* 2 (square max-n)) 1))
