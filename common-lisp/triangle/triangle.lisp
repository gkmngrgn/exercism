(in-package #:cl-user)
(defpackage #:triangle
  (:use #:cl)
  (:export #:triangle))

(in-package #:triangle)

(defun triangle (i j k)
  (let* ((sides (list i j k))
         (different-sides (remove-duplicates sides))
         (sum (reduce #'+ sides)))
    (labels ((count-invalid-sides (counter side)
               (if (<= (- sum side) side)
                   (1+ counter)
                   counter))
             (different-sides ()
               (if (zerop (reduce #'count-invalid-sides sides :initial-value 0))
                   (length different-sides))))
      (case (different-sides)
        (1         :equilateral)
        (2         :isosceles)
        (3         :scalene)
        (otherwise :illogical)))))
