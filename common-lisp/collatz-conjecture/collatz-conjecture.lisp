(defpackage #:collatz-conjecture
  (:use #:common-lisp)
  (:export #:collatz))

(in-package #:collatz-conjecture)

(defun collatz (n)
  (if (> n 0)
      (let ((step 0))
        (loop while (> n 1)
              do (progn (setf step (1+ step))
                        (setf n (if (evenp n)
                                    (/ n 2)
                                    (1+ (* n 3))))))
        step)))
