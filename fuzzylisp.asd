(asdf:defsystem #:fuzzylogic
  :author "Shaun Pearce"
  :version "0.1"
  :license "BSD-3 Clause"
  :description "Fuzzy Logic Toolkit in CL"
  :serial t
  :components ((:file "fuzzylisp"))
  :in-order-to ((asdf:test-op (asdf:test-op :fuzzylogic.tests))))
