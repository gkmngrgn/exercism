(in-package #:cl-user)
(defpackage #:bob
  (:use #:cl)
  (:export #:response))
(in-package #:bob)

(ql:quickload "str")

(defun response (hey-bob)
  (let ((message (str:trim hey-bob)))
    (cond ((str:empty? message)
           "Fine. Be that way!")
          ((str:ends-with? "?" message)
           (if (yelling? message)
               "Calm down, I know what I'm doing!"
               "Sure."))
          ((yelling? message)
           "Whoa, chill out!")
          (t "Whatever."))))

(defun yelling? (message)
  (and (loop for s in (coerce message 'list)
          when (str:alpha? (string s)) return t)
       (str:upcase? message)))
