(defpackage #:atbash-cipher
  (:use #:common-lisp)
  (:export #:encode))

(in-package #:atbash-cipher)

(ql:quickload "str")

(defun translate-letter (letter step)
  (format nil "~a~a"
          (if (str:digit? letter)
              letter
              (code-char (+ (char-code #\a) (- (char-code #\z) (char-code (char letter 0))))))
          (if (zerop (rem step 5)) " " "")))

(defun encode (plaintext)
  (let* ((step 1)
         (translated (loop for letter in (str:split "" (str:downcase plaintext))
                           if (str:alphanum? letter)
                             collect (translate-letter letter step)
                           do (if (str:alphanum? letter)
                              (setf step (1+ step))))))
    (str:trim-right (str:join "" translated))))
