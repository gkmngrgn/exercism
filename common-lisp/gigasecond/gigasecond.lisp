(in-package #:cl-user)
(defpackage #:gigasecond
  (:use #:cl)
  (:export #:from))
(in-package #:gigasecond)

(defun from (year month day hour minute second)
  (let* ((after-gigasecond (+ (expt 10 9)
                              (encode-universal-time second minute hour day month year 0)))
         (result (multiple-value-list (decode-universal-time after-gigasecond 0))))
    (subseq (reverse result) 3)))
