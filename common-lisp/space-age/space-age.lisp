(in-package #:cl-user)
(defpackage #:space-age
  (:use #:common-lisp)
  (:export #:on-earth
           #:on-mercury
           #:on-venus
           #:on-mars
           #:on-jupiter
           #:on-saturn
           #:on-uranus
           #:on-neptune))

(in-package #:space-age)

(defun age (seconds orbital-periods)
  (/ (/ seconds 31557600) orbital-periods))

(defun on-earth (seconds)
  (age seconds 1))

(defun on-mercury (seconds)
  (age seconds 0.2408467))

(defun on-venus (seconds)
  (age seconds 0.61519726))

(defun on-mars (seconds)
  (age seconds 1.8808158))

(defun on-jupiter (seconds)
  (age seconds 11.862615))

(defun on-saturn (seconds)
  (age seconds 29.447498))

(defun on-uranus (seconds)
  (age seconds 84.016846))

(defun on-neptune (seconds)
  (age seconds 164.79132))
