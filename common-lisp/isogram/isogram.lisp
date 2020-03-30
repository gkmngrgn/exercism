(defpackage #:isogram
  (:use #:cl)
  (:export #:is-isogram))

(in-package #:isogram)

(ql:quickload "str")

(defun is-isogram (string)
  (let* ((letter-list (loop for letter in (str:split "" (str:downcase string))
                         if (str:letters? letter)
                         collect letter))
         (letter-str (str:join "" letter-list)))
    (loop for letter in letter-list
       always (= (str:count-substring letter letter-str) 1))))
