(defpackage #:roman
  (:use #:cl)
  (:export #:romanize))

(in-package #:roman)

(defvar roman-numerals '((1000 . "M")
                         (900  . "CM")
                         (500  . "D")
                         (400  . "CD")
                         (100  . "C")
                         (90   . "XC")
                         (50   . "L")
                         (40   . "XL")
                         (10   . "X")
                         (9    . "IX")
                         (5    . "V")
                         (4    . "IV")
                         (1    . "I")))

(defun take-closest-numeral (number)
  (loop for n in roman-numerals
     when (<= (car n) number)
     return n))

(defun romanize (number)
  (if (= number 0) ""
      (let ((numeral (take-closest-numeral number)))
        (concatenate 'string
                     (cdr numeral)
                     (romanize (- number (car numeral)))))))
