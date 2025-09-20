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

(defun string-to-boolean (name ty x)
  (let ((value (string-downcase x)))
    (if (string-equal "true" value)
        t
        (if (string-equal "false" value)
            nil
            (raise-invalid-environment-variable name ty x)))))

(defun string-to-integer (name ty x)
    (handler-case
        (let ((value (parse-integer x)))
          (if (not value)
              (raise-invalid-environment-variable name ty x)
              value))
      (t (err)
        (declare (ignore err))
        (raise-invalid-environment-variable name ty x))))

(defun string-as-string (name ty x)
  (declare (ignore name ty))
  x)

(define-type-parser :string #'string-as-string)
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

(defun raise-invalid-environment-variable (name ty value)
  (error 'invalid-environment-variable
         :format-control "~%[cl-environment-variables] `~A` is not of type `~A`. Value is `~A'.~%"
         :format-arguments (list name ty value)))

(defun validate-env-type (name ty value  &optional default-value)
  (let ((fn (gethash ty types-table)))
    (if (and (not value) default-value)
        default-value
        (funcall fn name ty value))))

(defvar *env-specs* (make-hash-table :test 'equal))

(defun define-env-var (name type env-name &optional default-value)
  (setf (gethash name *env-specs*)
        (validate-env-type name
                           type
                           (uiop:getenv env-name)
                           default-value)))

(defmacro define-env-vars (&rest vars)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@(map nil
            (lambda (v)
              (destructuring-bind (name type &rest props)
                  v
                (declare (ignorable props))
                (let* ((default-value (getf props :default nil))
                       (real-name (string-upcase (string name)))
                       (env-name (substitute #\_ #\- real-name)))
                  `(define-env-var ,name ,type ,env-name ,default-value))))
            vars))
  t)

(defun environment-variable (name)
  (gethash name *env-specs*))
