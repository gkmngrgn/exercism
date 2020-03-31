(in-package #:cl-user)
(defpackage #:pascal
  (:use #:cl)
  (:export #:rows))
(in-package #:pascal)

(defun rows (n)
  (reduce (lambda (rows i)
            (if (zerop i)
                (list (list 1))
                (let* ((last-row (car (last rows)))
                       (row (loop for x in (append last-row (list 0))
                               for y in (append (list 0) last-row)
                               collect (+ x y))))
                  (append rows (list row)))))
          (loop for i from 0 below n collect i)
          :initial-value (list)))