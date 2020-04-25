(defpackage #:sieve
  (:use #:cl)
  (:export #:primes-to)
  (:documentation "Generates a list of primes up to a given limit."))

(in-package #:sieve)

(defun primes-to (upper-bound)
  (if (> upper-bound 1)
      (let ((prime-table (loop repeat (1+ upper-bound) collect t)))
        (loop for index from 2 to (sqrt upper-bound)
              do (if (nth index prime-table)
                     (loop for number from (expt index 2) to upper-bound
                           by index
                           do (setf (nth number prime-table) nil))))
        (nthcdr 2 (loop for index below (length prime-table)
                        for is-prime in prime-table
                        if is-prime
                          collect index)))))
