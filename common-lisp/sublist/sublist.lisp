(defpackage #:sublist
  (:use #:common-lisp)
  (:export #:sublist))

(in-package #:sublist)

(defun sublist (list1 list2)
  (labels ((is-sublist (l1 l2)
             (cond ((or (null l2)
                        (< (length l2) (length l1))) nil)
                   ((equal l1 (subseq l2 0 (length l1))) t)
                   (t (is-sublist l1 (cdr l2))))))
    (cond ((equal list1 list2)      :equal)
          ((is-sublist list1 list2) :sublist)
          ((is-sublist list2 list1) :superlist)
          (t                        :unequal))))
