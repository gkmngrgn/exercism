(defpackage #:collatz-conjecture
  (:use #:common-lisp)
  (:export #:collatz))

(in-package #:collatz-conjecture)

(defun collatz (n)
  (if (> n 0)
      (loop while (> n 1)
            counting t
            do (setf n (if (evenp n)
                           (/ n 2)
                           (1+ (* n 3)))))))
