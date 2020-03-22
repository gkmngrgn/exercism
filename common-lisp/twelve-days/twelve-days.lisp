(in-package #:cl-user)
(defpackage #:twelve-days
  (:use #:cl)
  (:export #:recite))

(in-package #:twelve-days)

(defvar days-and-goods
  '("a Partridge in a Pear Tree"
    "two Turtle Doves"
    "three French Hens"
    "four Calling Birds"
    "five Gold Rings"
    "six Geese-a-Laying"
    "seven Swans-a-Swimming"
    "eight Maids-a-Milking"
    "nine Ladies Dancing"
    "ten Lords-a-Leaping"
    "eleven Pipers Piping"
    "twelve Drummers Drumming"))

(defun recite (&optional begin end)
  (labels ((verse (index)
             (format nil
                     "On the ~:r day of Christmas my true love gave to me: ~a."
                     index (get-goods index)))
           (get-goods (index)
             (reduce (lambda (all-goods i)
                       (let ((goods (nth (- i 1) days-and-goods)))
                         (if (null all-goods)
                             goods
                             (format nil "~a,~a~a" all-goods (if (= i 1) " and " " ") goods))))
                     (loop for i from index downto 1 collect i)
                     :initial-value nil)))
    (format nil
            "~{~&~a~}"
            (let ((begin (or begin 1))
                  (end (or end (or begin 12))))
              (loop for i from begin to end collect (verse i))))))
