(in-package #:cl-user)
(defpackage #:word-count
  (:use #:cl)
  (:export #:count-words))
(in-package #:word-count)

(ql:quickload "cl-ppcre")
(ql:quickload "str")

(defun count-words (sentence)
  (let* ((words (split-words sentence))
         (unique-words (remove-duplicates words :test #'equal)))
    (loop for word in unique-words
       collect (cons word (count word words :test #'equal)))))

(defun split-words (sentence)
  (mapcar (lambda (w) (string-trim "'" w))
          (str:words (cl-ppcre:regex-replace-all "[^a-z0-9']"
                                                 (str:downcase sentence)
                                                 " "))))
