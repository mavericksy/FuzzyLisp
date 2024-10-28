(asdf:defsystem #:fuzzylogic.tests
  :author "Shaun Pearce"
  :description "FuzzyLogic Tests"
  :depends-on (#:fuzzylogic
               #:fiveam)
  :components ((:file "fuzzylisptests"))
  :serial t
  :perform (asdf:test-op (o c)
                         (uiop:symbol-call 
                          :fuzzylogic.tests
                          :run-all-tests)))
