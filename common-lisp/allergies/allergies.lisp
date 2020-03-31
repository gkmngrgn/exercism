(in-package #:cl-user)
(defpackage #:allergies
  (:use #:common-lisp)
  (:shadow #:list)
  (:export #:allergic-to-p #:list))

(in-package #:allergies)

(defvar *allergens* '(("eggs"         . 1)
                      ("peanuts"      . 2)
                      ("shellfish"    . 4)
                      ("strawberries" . 8)
                      ("tomatoes"     . 16)
                      ("chocolate"    . 32)
                      ("pollen"       . 64)
                      ("cats"         . 128)))

(defun allergic-to-p (score allergen)
  (loop for a in *allergens*
     when (string= (string-downcase allergen) (car a))
     return (> (logand (cdr a) score) 0)))

(defun list (score)
  (mapcar #'car
          (remove-if-not (lambda (a)
                           (allergic-to-p score (car a)))
                         *allergens*)))
