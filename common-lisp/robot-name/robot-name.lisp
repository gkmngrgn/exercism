(in-package #:cl-user)
(defpackage #:robot
  (:use #:common-lisp)
  (:export #:build-robot #:robot-name #:reset-name))

(in-package #:robot)

(defvar *robot-names* (make-hash-table))

(defun generate-name ()
  (let* ((codes (loop for i from 1
                   upto 5
                   collect (if (< i 3)
                               (+ (random 26) 65)
                               (+ (random 10) 48))))
         (name (coerce (mapcar #'code-char codes) 'string)))
    (if (gethash name *robot-names*)
        (generate-name)
        name)))

(defclass robot ()
  ((name :reader robot-name
         :initarg :name
         :accessor name)))

(defmethod reset-name ((obj robot))
  (setf (name obj) (generate-name)))

(defun build-robot ()
  (let ((robot (make-instance 'robot :name (generate-name))))
    (setf (gethash (robot-name robot) *robot-names*) t)
    robot))
