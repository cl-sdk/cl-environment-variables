(defpackage #:cl-environment-variables.test
  (:use #:cl))

(in-package :cl-environment-variables.test)

(5am:def-suite :cl-environment-variables.types.suite)

(5am:in-suite :cl-environment-variables.types.suite)

(5am:def-test raise-if-invalid-boolean ()
  (handler-case
      (cl-environment-variables::string-to-boolean :x :boolean "")
    (t (err)
      (5am:is-true (equal
                    'cl-environment-variables::invalid-environment-variable
                    (type-of err))))))

(5am:def-test must-allow-uppercase ()
  (5am:is-true (cl-environment-variables::string-to-boolean :x :boolean "TRUE"))
  (5am:is-false (cl-environment-variables::string-to-boolean :x :boolean "FALSE")))

(5am:def-test raise-if-invalid-integer ()
  (handler-case
      (cl-environment-variables::string-to-integer :x :integer "")
    (t (err)
      (5am:is-true (equal
                    'cl-environment-variables::invalid-environment-variable
                    (type-of err))))))

(5am:def-test raise-if-string-has-junk ()
  (handler-case
      (cl-environment-variables::string-to-integer :x :integer "asdf 1")
    (t (err)
      (5am:is-true (equal
                    'cl-environment-variables::invalid-environment-variable
                    (type-of err))))))

(5am:def-test must-successifully-parse-number ()
  (5am:is (= 1 (cl-environment-variables::string-to-integer :x :integer "1"))))

(5am:def-test raise-if-invalid-string ()
  (handler-case
      (cl-environment-variables::string-as-string :x :string "")
    (t (err)
      (5am:is-true (equal
                    'cl-environment-variables::invalid-environment-variable
                    (type-of err))))))

(5am:def-test must-successifully-get-the-string ()
  (5am:is (string-equal "ok"
                        (cl-environment-variables::string-as-string :x
                                                                     :integer
                                                                     "ok"))))

(5am:def-suite :cl-environment-variables.suite)

(5am:in-suite :cl-environment-variables.suite)
