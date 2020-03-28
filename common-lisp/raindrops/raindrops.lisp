(defpackage #:raindrops
  (:use #:common-lisp)
  (:export #:convert))

(in-package #:raindrops)

(defun convert (value)
  (let ((result (format nil "~a~a~a"
                        (if (zerop (rem value 3)) "Pling" "")
                        (if (zerop (rem value 5)) "Plang" "")
                        (if (zerop (rem value 7)) "Plong" ""))))
    (if (string= result "") (format nil "~d" value) result)))
