(defpackage #:squares
  (:use #:cl)
  (:export #:sum-of-squares
           #:square-of-sum
           #:difference))

(in-package #:squares)

(defun sum-of-squares (number)
  (reduce #'+ (loop for i from 1 upto number collect (* i i))))

(defun square-of-sum (number)
  (let ((sum (reduce #'+ (loop for i from 1 upto number collect i))))
    (* sum sum)))

(defun difference (number)
  (- (square-of-sum number) (sum-of-squares number)))
