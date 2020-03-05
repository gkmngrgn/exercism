(in-package #:cl-user)
(defpackage #:beer
  (:use #:common-lisp)
  (:export #:verse #:sing))

(in-package #:beer)

(defun verse (n)
  (cond ((= n 0) (format nil
                         "No more bottles of beer on the wall, no more bottles of beer.~&~
                          Go to the store and buy some more, 99 bottles of beer on the wall.~&"))
        ((= n 1) (format nil
                         "1 bottle of beer on the wall, 1 bottle of beer.~&~
                          Take it down and pass it around, no more bottles of beer on the wall.~&"))
        (t (let ((message1 (format nil "~d bottles of beer on the wall, ~d bottles of beer." n n))
                 (message2 (cond ((= n 2) "Take one down and pass it around, 1 bottle of beer on the wall.")
                                 (t (format nil "Take one down and pass it around, ~d bottles of beer on the wall." (- n 1))))))
             (format nil "~a~&~a~&" message1 message2)))))

(defun sing (start &optional (end 0))
  (format nil "~a~%~a"
          (verse start)
          (if (= start end) "" (sing (- start 1) end))))
