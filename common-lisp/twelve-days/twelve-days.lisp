(in-package #:cl-user)
(defpackage #:twelve-days
  (:use #:cl)
  (:export #:recite))

(in-package #:twelve-days)

(defvar days-and-goods
  '(("first"    . "a Partridge in a Pear Tree")
    ("second"   . "two Turtle Doves")
    ("third"    . "three French Hens")
    ("fourth"   . "four Calling Birds")
    ("fifth"    . "five Gold Rings")
    ("sixth"    . "six Geese-a-Laying")
    ("seventh"  . "seven Swans-a-Swimming")
    ("eigth"    . "eight Maids-a-Milking")
    ("nineth"   . "nine Ladies Dancing")
    ("tenth"    . "ten Lords-a-Leaping")
    ("eleventh" . "eleven Pipers Piping")
    ("twelfth"  . "twelve Drummers Drumming")))

(defun recite (&optional begin end)
  (labels ((verse (index)
             (format nil
                     "On the ~a day of Christmas my true love gave to me: ~a"
                     (get-ordinal-numeral-as-str index)
                     (get-goods-list index)))
           (get-ordinal-numeral-as-str (index)
             (car (nth (- index 1) days-and-goods)))
           (get-goods (index)
             (if (= index 1)
                 (cdr (nth 1 days-and-goods))
                 (loop for ))
             (loop for i ...))
           ))
    (loop for i from begin
       below end
       collect (verse i))))
