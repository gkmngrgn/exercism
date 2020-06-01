(in-package #:cl-user)
(defpackage #:meetup
  (:use #:common-lisp)
  (:export #:meetup))

(in-package #:meetup)

(ql:quickload "local-time")

(defun get-weekday (weekday)
  (let ((weekdays '(:sunday :monday :tuesday :wednesday :thursday :friday :saturday)))
    (position weekday weekdays)))

(defun adjust-timestamp (day month year expected-weekday)
  (let* ((ts (local-time:encode-timestamp 0 0 0 0 day month year))
         (day-diff (rem (- (+ (get-weekday expected-weekday) 7)
                           (local-time:timestamp-day-of-week ts))
                        7)))
    (if (zerop day-diff)
        ts
        (adjust-timestamp (+ day day-diff) month year expected-weekday))))

(defun meetup (month year weekday ordinal)
  (labels ((find-day (day)
             (let* ((ts (adjust-timestamp day month year weekday)))
               (list (local-time:timestamp-year  ts)
                     (local-time:timestamp-month ts)
                     (local-time:timestamp-day   ts)))))
    (case ordinal
      (:first  (find-day 1))  ;; 0 * 7 + 1
      (:second (find-day 8))  ;; 1 * 7 + 1
      (:third  (find-day 15)) ;; 2 * 7 + 1
      (:fourth (find-day 22)) ;; 3 * 7 + 1
      (:teenth (find-day 13)) ;; from thirteen to nineteen
      (:last   (find-day (- (local-time:days-in-month month year) 6))))))
