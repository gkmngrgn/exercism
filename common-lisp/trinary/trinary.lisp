(defpackage #:trinary
  (:use #:common-lisp)
  (:export #:to-decimal))

(in-package #:trinary)

(defparameter *base* 3)

(defun to-decimal (input)
  (let ((binaries (reverse (mapcar #'digit-char-p (coerce input 'list))))
        (exponent 0))
    (reduce #'+ (loop for b in binaries
                      when (or (null b) (>= b *base*))
                        return '(0)
                      collect (* b (expt *base* exponent))
                      do (setf exponent (1+ exponent))))))
