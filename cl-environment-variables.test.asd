(asdf:defsystem #:cl-environment-variables.test
  :author "Bruno Dias"
  :license "Unlicense"
  :version "0.0.1"
  :depends-on (#:fiveam
               #:cl-environment-variables)
  :serial t
  :components ((:file "t/tests")))
