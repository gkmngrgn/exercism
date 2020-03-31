(defpackage #:scrabble-score
  (:use #:cl)
  (:export #:score-word))

(in-package #:scrabble-score)

(ql:quickload "str")

(defun score-word (word)
  (reduce (lambda (score letter)
            (+ score (cond ((not (str:letters? letter)) 0)
                           ((str:contains? letter "QZ") 10)
                           ((str:contains? letter "JX") 8)
                           ((str:contains? letter "K") 5)
                           ((str:contains? letter "FHVWY") 4)
                           ((str:contains? letter "BCMP") 3)
                           ((str:contains? letter "DG") 2)
                           (t 1))))
          (str:split "" (str:upcase word))
          :initial-value 0))
