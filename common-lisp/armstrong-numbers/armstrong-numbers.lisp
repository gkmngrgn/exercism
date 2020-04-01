(in-package #:cl-user)
(defpackage #:armstrong-numbers
  (:use #:cl)
  (:export #:armstrong-number-p))
(in-package #:armstrong-numbers)

(defun armstrong-number-p (number)
  (let* ((num number)
         (digits (loop while (> num 0)
                    collect (rem num 10)
                    do (setf num (car (multiple-value-list
                                       (floor num 10))))))
         (len (length digits))
         (sum (reduce (lambda (s d) (+ s (expt d len)))
                      digits
                      :initial-value 0)))
    (= number sum)))
