(in-package #:cl-user)
(defpackage #:anagram
  (:use #:common-lisp)
  (:export #:anagrams-for))

(in-package #:anagram)

(defun anagrams-for (word word-list)
  (labels ((is-anagram (first second)
             (let ((first (string-downcase first))
                   (second (string-downcase second)))
               (and (not (equal first second))
                    (equal (sort first #'char-lessp)
                           (sort second #'char-lessp)))))
           (anagrams-list (remaining-list anagrams)
             (if (null remaining-list)
                 anagrams
                 (let* ((current-word (car remaining-list))
                        (remaining-list (cdr remaining-list))
                        (anagrams (if (is-anagram word current-word)
                                      (append anagrams (list current-word))
                                      anagrams)))
                   (anagrams-list remaining-list anagrams)))))
    (anagrams-list word-list '())))
