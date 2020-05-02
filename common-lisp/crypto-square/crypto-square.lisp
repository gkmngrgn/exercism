(in-package #:cl-user)
(defpackage #:crypto-square
  (:use #:cl)
  (:export #:encipher))
(in-package #:crypto-square)

(ql:quickload "str")

(defun get-edges (length)
  (labels ((find-max-edges (r c)
             (cond ((>= (* r c) length) (values r c))
                   ((= r c) (find-max-edges r (1+ c)))
                   (t (find-max-edges (1+ r) c)))))
    (find-max-edges 1 1)))

(defun get-letters (plaintext)
  (loop for c in (str:split "" (str:downcase plaintext))
        if (str:alphanum? c)
          collect c))

(defun get-encrypted-letters (letters width height)
  (if (zerop (length letters))
      (list)
      (loop for h below height
            collect (loop for w below width
                          collect (or (nth (+ (* height w) h) letters)
                                      " ")))))

(defun encipher (plaintext)
  (let* ((letters (get-letters plaintext)))
    (multiple-value-bind (width height)
        (get-edges (length letters))
      (str:join " " (mapcar (lambda (line) (str:join "" line))
                            (get-encrypted-letters letters width height))))))
