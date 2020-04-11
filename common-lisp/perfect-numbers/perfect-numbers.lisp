(defpackage #:perfect-numbers
  (:use #:common-lisp)
  (:export #:classify))

(in-package #:perfect-numbers)

(defun classify (n)
  (cond ((< n 1) nil)
        (t (let* ((values (loop for i from 1 below n
                                if (zerop (rem n i))
                                  collect i))
                  (sum (reduce #'+ values)))
             (cond ((< sum n) "deficient")
                   ((> sum n) "abundant")
                   (t         "perfect"))))))
