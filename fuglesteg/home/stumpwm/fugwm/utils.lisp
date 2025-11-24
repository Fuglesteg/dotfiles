(uiop:define-package :fugwm/utils
  (:use :cl)
  (:export :string-case
           :trim-string
           :clamp))

(in-package :fugwm/utils)

(defmacro string-case (expression &rest forms)
  `(cond
     ,@(loop for form in forms
             collect (destructuring-bind (to-compare result) form
                       (if (eq to-compare t)
                           `(t ,result)
                           `((string= ,expression ,to-compare) ,result))))))

(defun trim-string (string-to-trim desired-length)
  (if (< (length string-to-trim) desired-length)
      string-to-trim
      (concat (subseq string-to-trim 0 desired-length) "-")))

(defun clamp (number min max)
  (cond ((< number min) min)
        ((> number max) max)
        (t number)))
