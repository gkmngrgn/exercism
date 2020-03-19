(in-package #:cl-user)
(defpackage #:acronym
  (:use #:common-lisp)
  (:export #:acronym))

(in-package #:acronym)

(ql:quickload "str")

(defun acronym (text)
  (labels ((is-acronym-letter (index)
             (let ((prev (if (= index 0)
                             " "
                             (str:s-nth (- index 1) text)))
                   (current (str:s-nth index text)))
               (and (str:letters? current)
                    (or (string= prev " ")
                        (string= prev "-")
                        (and (str:upcase? current)
                             (str:downcase? prev))))))
           (find-acronym-letters ()
             (loop for index from 0
                below (length text)
                when (is-acronym-letter index)
                collect (str:upcase (str:s-nth index text)))))
    (str:join "" (find-acronym-letters))))
