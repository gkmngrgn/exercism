(in-package #:cl-user)
(defpackage #:binary-search
  (:use #:common-lisp)
  (:export #:binary-find #:value-error))

(in-package #:binary-search)

(defun binary-find (arr el)
  (labels ((binary-find-index (low high)
             (when (< low high)
               (let* ((index (floor (+ low high) 2))
                      (current-el (elt arr index)))
                 (cond ((= current-el el) index)
                       ((> current-el el) (binary-find-index low index))
                       (t (binary-find-index (1+ index) high)))))))
    (binary-find-index 0 (length arr))))
