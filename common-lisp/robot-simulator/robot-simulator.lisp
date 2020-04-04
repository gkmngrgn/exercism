(in-package #:cl-user)
(defpackage #:robot-simulator
  (:use #:common-lisp)
  (:export #:north #:east #:south #:west #:execute-sequence
	   #:robot #:robot-position #:robot-bearing #:make-robot))

(in-package #:robot-simulator)

(defvar north "north")
(defvar east  "east")
(defvar south "south")
(defvar west  "west")

(defclass robot ()
  ((position :reader robot-position
             :initarg :position
             :accessor pos)
   (bearing :reader robot-bearing
            :initarg :bearing
            :accessor bearing)))

(defun make-robot (&key (position '(0 . 0)) (bearing north))
  (make-instance 'robot :position position :bearing bearing))

(defun turn-left (robot)
  (let ((bearing (robot-bearing robot)))
    (setf (bearing robot) (cond ((string= bearing north) west)
                                ((string= bearing west) south)
                                ((string= bearing south) east)
                                ((string= bearing east) north)))))

(defun turn-right (robot)
  (let ((bearing (robot-bearing robot)))
    (setf (bearing robot) (cond ((string= bearing north) east)
                                ((string= bearing east) south)
                                ((string= bearing south) west)
                                ((string= bearing west) north)))))

(defun advance (robot)
  (let ((x (car (robot-position robot)))
        (y (cdr (robot-position robot)))
        (bearing (robot-bearing robot)))
    (setf (pos robot) (cond ((string= bearing north) (cons x (+ y 1)))
                            ((string= bearing east) (cons (+ x 1) y))
                            ((string= bearing south) (cons x (- y 1)))
                            ((string= bearing west) (cons (- x 1) y))))))

(defun execute-sequence (robot directions)
  (mapcar (lambda (direction)
            (cond ((char= direction #\L) (turn-left robot))
                  ((char= direction #\R) (turn-right robot))
                  ((char= direction #\A) (advance robot))))
          (coerce directions 'list)))
