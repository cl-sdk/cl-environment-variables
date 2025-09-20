(defpackage #:cl-environment-variables
  (:use #:cl)
  (:export
   #:environment-variable
   #:define-env-vars
   #:define-type-parser
   #:raise-invalid-environment-variable))

(in-package :cl-environment-variables)

(defvar types-table (make-hash-table :test 'equal))

(defun define-type-parser (ty parser)
  (setf (gethash ty types-table) parser))

(defun string-to-boolean (x)
  (let ((value (string-downcase x)))
    (if (member value '("true" "false"))
        (string-equal (string-downcase x) "true")
        (raise-invalid-environment-variable :boolean x))))

(defun string-to-integer (x)
  (handler-case
      (let ((value (parse-integer x)))
        (if value
            (string-equal (string-downcase x) "true")
            (raise-invalid-environment-variable :integer x)))
    (t (err)
      (declare (ignore err))
      (raise-invalid-environment-variable :integer x))))

(define-type-parser :string #'identity)
(define-type-parser :integer #'string-to-integer)
(define-type-parser :boolean #'string-to-boolean)

(defun invalid-environment-variable-printer (condition stream)
  (let ((control (simple-condition-format-control condition)))
    (if control
        (apply #'format stream
               control
               (simple-condition-format-arguments condition))
        (error "No format-control for ~S" condition))))

(define-condition invalid-environment-variable (simple-condition)
  ()
  (:report invalid-environment-variable-printer))

(defun raise-invalid-environment-variable (ty value)
  (error 'invalid-environment-variable
         (format nil "~%[cl-environment-variables] `~A` is not of type `~A`. Value is `~A'.~%" name ty x)))

(defun validate-env-type (x ty &optional default-value)
  (let ((fn (gethash ty types-table)))
    (handler-case
        (if (and (not x) default-value)
            default-value
            (funcall fn x))
      (t (err)
        (declare (ignore err))
        (error err)))))

(defvar *env-specs* (make-hash-table :test 'equal))

(defmacro define-env-vars (&rest vars)
  (map nil
       (lambda (v)
         (destructuring-bind (name type &rest props)
             v
           (declare (ignorable props))
           (let* ((default-value (getf props :default nil))
                  (real-name (string-upcase (string name)))
                  (env-name (substitute #\_ #\- real-name)))
             (setf (gethash name *env-specs*)
                   (validate-env-type name
                                      (uiop:getenv env-name)
                                      type
                                      default-value)))))
       vars)
  t)

(defun environment-variable (name)
  (gethash name *env-specs*))
