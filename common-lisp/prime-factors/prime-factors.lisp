(defpackage #:prime-factors
  (:use #:common-lisp)
  (:export :factors-of))

(in-package #:prime-factors)

(defun factors-of (n)
  (let ((num n)
        (div 2))
    (loop while (not (= num 1))
          when (zerop (rem num div))
            collect div
          if (zerop (rem num div))
            do (setf num (/ num div))
          else
            do (setf div (+ div 1)))))
