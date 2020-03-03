(in-package #:cl-user)
(defpackage #:rna-transcription
  (:use #:cl)
  (:export #:to-rna))
(in-package #:rna-transcription)

(defun get-rna-code (code)
  (cond ((char= code #\G) #\C)
        ((char= code #\C) #\G)
        ((char= code #\T) #\A)
        ((char= code #\A) #\U)
        (t (error "invalid dna code"))))

(defun to-rna (str)
  (concatenate 'string
               (loop for code in (coerce str 'list)
                  collect (get-rna-code code))))
