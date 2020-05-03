(defpackage #:strain
  (:use #:common-lisp)
  (:export #:keep #:discard))

(in-package #:strain)

(defun keep (fn list)
  (loop for i below (length list)
        for l in (mapcar fn list)
        if (not (null l))
          collect (nth i list)))

(defun discard (fn list)
  (loop for i below (length list)
        for l in (mapcar fn list)
        if (null l)
          collect (nth i list)))
