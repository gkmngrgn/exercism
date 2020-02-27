(defpackage #:leap
  (:use #:common-lisp)
  (:export #:leap-year-p))
(in-package #:leap)

(defun leap-year-p (year)
  (and
   (= (rem year 4) 0)
   (or (not (= (rem year 100) 0))
       (= (rem year 400) 0))))
