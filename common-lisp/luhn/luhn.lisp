(in-package #:cl-user)
(defpackage #:luhn
  (:use #:cl)
  (:export #:is-valid))

(in-package #:luhn)

(ql:quickload "str")

(defun sum-of-digits-by-luhn (digits)
  (let* ((digits (mapcar #'digit-char-p digits))
         (sums (mapcar (lambda (index digit)
                         (cond ((zerop (rem index 2)) digit)
                               ((< digit 5) (* digit 2))
                               (t (- (* digit 2) 9))))
                       (loop for i from 0 below (length digits)
                          collect i)
                       (reverse digits))))
    (reduce #'+ sums)))

(defun is-valid (input)
  (let ((code (str:replace-all " " "" input))
        (digits (remove-if-not (lambda (i) (str:digitp (string i))) (coerce input 'list))))
    (if (and (= (length code) (length digits))
             (> (length digits) 1))
        (zerop (rem (sum-of-digits-by-luhn digits) 10))
        nil)))
