(defpackage #:cl-environment-variables
  (:use #:cl)
  (:export
   #:environment-variable
   #:define-env-vars
   #:define-type-parser))

(in-package :cl-environment-variables)

(defvar types-table (make-hash-table :test 'equal))

(defun define-type-parser (ty parser)
  (setf (gethash ty types-table) parser))

(defun string-to-boolean (x)
  (string-equal (string-downcase x) "true"))

(define-type-parser :string #'identity)
(define-type-parser :integer #'parse-integer)
(define-type-parser :boolean #'string-to-boolean)

(defun validate-env-type (name x ty &optional default-value)
  (let ((fn (gethash ty types-table)))
    (handler-case
        (if (and (not x) default-value)
            default-value
            (funcall fn x))
      (t (err)
        (declare (ignore err))
        (format t "[cl-environment-variables] `~A` is not of type `~a`. Value is `~A'.~%" name ty x)))))

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
