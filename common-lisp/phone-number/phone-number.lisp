(in-package #:cl-user)
(defpackage #:phone
  (:use #:common-lisp)
  (:export #:numbers #:area-code #:pretty-print))

(in-package #:phone)

(ql:quickload "str")

(defun numbers (phone-number)
  (let ((digits (loop for char in (str:split "" phone-number)
                      if (str:digit? char)
                      collect char)))
    (cond ((= (length digits) 10)
           (str:join "" digits))
          ((and (= (length digits) 11)
                (string= (car digits) "1"))
           (str:join "" (cdr digits)))
          (t (str:join "" (loop repeat 10 collect "0"))))))

(defun area-code (phone-number)
  (let* ((phone-number (numbers phone-number))
         (code-numbers(loop for i below 3
                            collect (str:s-nth i phone-number))))
    (str:join "" code-numbers)))

(defun pretty-print (phone-number)
  (let* ((digits (numbers phone-number))
         (digits (str:insert "(" 0 digits))
         (digits (str:insert ") " 4 digits))
         (digits (str:insert "-" 9 digits)))
    digits))
