(defpackage #:school
  (:use #:common-lisp)
  (:export #:make-school #:add #:grade-roster #:grade #:sorted))

(in-package #:school)

(defclass school ()
  ((grades :reader school-grades
           :initarg :grades)))

(defmethod add ((obj school) student student-grade)
  (let ((students (grade obj student-grade)))
    (setf (gethash student-grade (school-grades obj))
          (push student students))))

(defmethod grade-roster ((obj school))
  (let ((grade-roster (list)))
    (maphash (lambda (grade students)
               (push (list :grade grade :students students) grade-roster))
             (school-grades obj))
    grade-roster))

(defmethod grade ((obj school) student-grade)
  (or (gethash student-grade (school-grades obj)) (list)))

(defmethod sorted ((obj school))
  (labels ((sorting-func (a b)
             (< (nth 1 a) (nth 1 b))))
    (sort (grade-roster obj) #'sorting-func)))

(defun make-school ()
  (let ((grades (make-hash-table)))
    (make-instance 'school :grades grades)))
