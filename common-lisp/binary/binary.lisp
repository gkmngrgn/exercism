(defpackage #:binary
  (:use :common-lisp)
  (:export :to-decimal))

(in-package :binary)

(defun to-decimal (input)
  (let* ((exponent 0)
         (binaries (reverse (mapcar #'digit-char-p (coerce input 'list)))))
    (reduce #'+ (loop for b in binaries
                      if (and (not (null b))
                              (< b 2))
                        collect (* b (expt 2 exponent))
                        and do (setf exponent (1+ exponent))))))
