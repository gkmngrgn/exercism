(in-package #:cl-user)
(defpackage #:etl
  (:use #:common-lisp)
  (:export #:transform))

(in-package #:etl)

(defun transform (table)
  (let ((transformed-table (make-hash-table :test 'equalp)))
    (maphash (lambda (score words)
               (loop for word in words
                     do (setf (gethash word transformed-table) score)))
             table)
    transformed-table))
