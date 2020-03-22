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
    ("eighth"   . "eight Maids-a-Milking")
    ("ninth"    . "nine Ladies Dancing")
    ("tenth"    . "ten Lords-a-Leaping")
    ("eleventh" . "eleven Pipers Piping")
    ("twelfth"  . "twelve Drummers Drumming")))

(defun recite (&optional begin end)
  (labels ((verse (index)
             (format nil
                     "On the ~a day of Christmas my true love gave to me: ~a."
                     (get-ordinal-numeral-as-str index)
                     (get-goods index)))
           (get-ordinal-numeral-as-str (index)
             (car (nth (- index 1) days-and-goods)))
           (get-goods (index)
             (reduce (lambda (all-goods i)
                       (let ((goods (cdr (nth (- i 1) days-and-goods))))
                         (if (null all-goods)
                             goods
                             (format nil "~a,~a~a" all-goods (if (= i 1) " and " " ") goods))))
                     (loop for i from index
                        downto 1
                        collect i)
                     :initial-value nil)))
    (format nil
            "~{~&~a~}"
            (let ((begin (or begin 1))
                  (end (or end (or begin 12))))
              (loop for i from begin
                 to end
                 collect (verse i))))))
