(defpackage #:hamming
  (:use #:cl)
  (:export #:distance))

(in-package #:hamming)

(defun count-distance (strand1 strand2 count)
  (if (or (string= strand1 "") (string= strand2 ""))
      count
      (let ((s1 (char strand1 0))
            (s2 (char strand2 0)))
        (count-distance (subseq strand1 1)
                        (subseq strand2 1)
                        (+ count (if (char= s1 s2) 0 1))))))

(defun distance (dna1 dna2)
  (if (= (length dna1) (length dna2))
      (count-distance dna1 dna2 0)
      nil))
