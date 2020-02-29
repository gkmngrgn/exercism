(in-package #:cl-user)
(defpackage #:dna
  (:use #:common-lisp)
  (:export #:dna-count #:nucleotide-counts #:invalid-nucleotide))

(in-package #:dna)

(defvar nucleotides '(#\G #\C #\T #\A))

(define-condition invalid-nucleotide (error)
  ())

(defun dna-count (nucleotid dna)
  (if (null (find nucleotid nucleotides))
      (error 'invalid-nucleotide)
      (reduce (lambda (sum n) (+ sum (if (char= n nucleotid) 1 0)))
              (coerce dna 'list)
              :initial-value 0)))

(defun nucleotide-counts (dna)
  (let ((n-table (make-hash-table)))
    (loop for n in nucleotides
       do (setf (gethash n n-table) (dna-count n dna)))
    n-table))
