(defpackage #:binary
  (:use :common-lisp)
  (:export :to-decimal))

(in-package :binary)

(defparameter *base* 2)

(defun to-decimal (input)
  (let ((binaries (reverse (mapcar #'digit-char-p (coerce input 'list))))
        (exponent 0))
    (reduce #'+ (loop for b in binaries
                      if (and (not (null b)) (< b *base*))
                        collect (* b (expt *base* exponent))
                        and do (setf exponent (1+ exponent))))))
