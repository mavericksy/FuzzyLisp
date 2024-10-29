(in-package #:cl-user)
(defpackage #:fuzzylogic.tests
  (:use #:cl #:fiveam #:fuzzylogic)
  (:local-nicknames (#:f #:fuzzylogic)))
;;;
(in-package #:fuzzylogic.tests)
;;;
;;;
;;; Test the classical sets
;;;
;;;
(def-suite crisp-sets)

;;
(def-suite classical-set-properties :in crisp-sets :description "Test all classical set properties")
(def-suite classical-set-operations :in crisp-sets :description "Test all classical set operations")

(in-suite classical-set-properties)

(defvar cE '())
(defvar cU '(0 1 2 3 4 5 6 7 8 9))
(defvar cA '(1 3 5 7))
(defvar cB '(5 6 7 8 9))
(defvar cC '(0 1 2 3 4))

(test identity-classical
  "Test classical set identity
  A u 0 = A
  A u U = U
  A n U = A
  A n 0 = 0
"
  (is (equalp cA (union cA cE))                   "A u 0 != A")
  (is (equalp cU (sort (union cA cU) #'<))        "A u U != U")
  (is (equalp cA (sort (intersection cA cU) #'<)) "A n U != A")
  (is (equalp cE (sort (intersection cA cE) #'<)) "A n 0 != 0")
  )

(test idempotent-classical
  "Test classical set idempotency
  A u A = A
  A n A = A
  A n 0 = 0
"
  (is (equalp cA (sort (union cA cA) #'<))        "A u A != A")
  (is (equalp cA (sort (intersection cA cA) #'<)) "A n A != A")
  (is (equalp cE (sort (intersection cA cE) #'<)) "A n 0 != 0")
  )

(test complement-classical
  "Test classical set complements
  A u A' = U
  A n A' = 0
"
  (is (equalp cU (sort (union cA (f:setcomplement? cA cU)) #'<))
      "A u A' != U")
  (is (equalp cE (sort (intersection cA (f:setcomplement? cA cU)) #'<))
      "A n A' != 0")
  )

(test associative-classical
  "Test classical set associativity
  (A u B) u C = A u (B u C)
  (A n B) n C = A n (B n C)
"
  (is (equalp (sort (union (union cA cB) cC) #'<)
              (sort (union cA (union cB cC)) #'<))
      "(A u B) u C != A u (B u C)")
  ;;
  (is (equalp (sort (intersection (intersection cA cB) cC) #'<)
              (sort (intersection cA (intersection cB cC)) #'<))
      "(A n B) n C != A n (B n C) != 0")
  )

(test commutative-classical
  "Test classical set communtativity
  A u B = B u A
  A n B = B n A
"
  (is (equalp (sort (union cA cB) #'<)
              (sort (union cB cA) #'<))
      "A u B != B u A")
  (is (equalp (sort (intersection cA cB) #'<)
              (sort (intersection cB cA) #'<))
      "A n B != B n A")
  )

(test distributive-classical
  "Test classical set distributivity
  A u (B n C) = (A u B) n (A u C)
  A n (B u C) = (A n B) u (A n C)
"
  (is (equalp (sort (union cA (intersection cB cC)) #'<)
              (sort (intersection (union cA cB) (union cA cC)) #'<))
      "A u (B n C) != (A u B) n (A u C)")
  (is (equalp (sort (intersection cA (union cB cC)) #'<)
              (sort (union (intersection cA cB) (intersection cA cC)) #'<))
      "A n (B u C) != (A n B) u (A n C)")
  )

(test demorgans-classical
  "Test classical set De Morgans Laws
  (A u B)' = A' n B'
  (A n B)' = A' u B'
"
  (is (equalp (sort (f:setcomplement? (union cA cB) cU) #'<)
              (sort (intersection (f:setcomplement? cA cU)
                                  (f:setcomplement? cB cU)) #'<))
      "(A u B)' = A' n B'")
  (is (equalp (sort (f:setcomplement? (intersection cA cB) cU) #'<)
              (sort (union (f:setcomplement? cA cU)
                           (f:setcomplement? cB cU)) #'<))
      "(A n B)' = A' u B'")
  )
;;
;;
(run! 'classical-set-properties)
;;
;;
(in-suite classical-set-operations)
;;
(defvar U '(42 70 100 1 2 3 4 5 67))
(defvar A '(42 70 100))
(defvar B '(43 71 101))
(defvar C '(43 70 1))

(test setcomplement?-classical
  "Test the complement of sets"
  (is (equalp '(1 2 3 4 5 67) (f:setcomplement? A U))
      "Set complement must be '(1 2 3 4 5 67)")
  (is (equalp '()             (f:setcomplement? '() '()))
      "Empty setcomplement is empty set")
  )

(test union-classical
  "Test the union properties of sets"
  (is (equalp '()                      (union '() '()))
      "Union of empty sets is empty set")
  (is (equal '(67 5 4 3 2 1 42 70 100) (union A U))
      "Union A with U must be U")
  (is (equalp A                        (union A A))
      "Union A must be A")
  (is (equalp '(42 70 100 43 71 101)   (union (reverse A) B))
      "Union must be '(42 70 100 43 71 101)")
  (is (equalp A                        (union A '()))
      "Union with empty set must be A")
  )

(test intersect-classical
  "Test the intersection properties of sets"
  (is (equalp '()          (intersection A '()))
      "Intersection should be empty set")
  (is (equalp '(42 70 100) (intersection (reverse A) A))
      "A intersection with itself should be A")
  )

(test belongs-classical
  "Test membership of an element to a set"
  (is-true  (f:belongs '() '()) "Empty Sets contains The Empty Set")
  (is-true  (f:belongs '(42) U) "El 42 should be in U")
  (is-false (f:belongs '(101) U) "El 101 should not be in U")
  )

(test subset-classical
  "Test if set A is a subset of B"
  (is-true (f:subset '() U) "Empty sets are subsets")
  (is-true (f:subset U U) "U is a subset of itself")
  (is-true (f:subset A U) "A is a subset of U")
  )

(test cardinality?-classical
  "Test the cardinality of sets"
  (is (= 9 (f:cardinality U)) "U should have cardinality 9")
  (is (= 3 (f:cardinality A)) "A should have cardinality 3")
  )

(test equivalent-classical
  "Test the equivalence of sets"
  (is-true (f:equivalent U U) "U should be equivalent to itself")
  (is-true (f:equivalent A B) "A should be equivalent to B")
  )

(test eql-classical
  "Test whether sets are equal"
  (is-true  (f:eql-c '() '()) "Empty sets are equal")
  (is-true  (f:eql-c A A) "A set should be equal to itself")
  (is-true  (f:eql-c B B) "B set should be equal to itself")
  (is-false (f:eql-c A B) "A is not equla to B")
  )

(test disjoint-classical
  "Test the unequalness of sets"
  (is-true  (f:disjoint '() '()) "Empty sets are disjoint")
  (is-true  (f:disjoint A B) "A should be disjoint from B")
  (is-false (f:disjoint A A) "A is not dijoint from itself")
  (is-false (f:disjoint A C) "A is not disjoint from C")
  )

(defvar DE '(D E))
(defvar FG '(1 2))

(test cartesian-product?-classical
  "Test cartesian products of sets"
  (is (equalp '()                        (f:cartesian-product? '() '()))
      "Products of empty sets must be an empty set")
  (is (equalp '((D 1) (D 2) (E 1) (E 2)) (f:cartesian-product? DE FG)) "")
  (is (equalp '((1 D) (1 E) (2 D) (2 E)) (f:cartesian-product? FG DE)) "")
  )
;;
(run! 'crisp-sets)
;;;
;;;
;;; Test the fuzzy sets
;;;
;;;
(def-suite fuzzy-sets)
(in-suite fuzzy-sets)

(test belongs?-fuzzy
  "Test direct set membership return membership degree else nil"
  (is-false     (f:belongs?-fuzzy 'A '((C 0) (D 1)))
                "(A 1) is not is set {(C 0) (D 1)}")
  (is (equalp 0 (f:belongs?-fuzzy 'C '((C 0) (D 1))))
      "(C 0) is not is set {(C 0) (D 1)}")
  (is (equalp 1 (f:belongs?-fuzzy 'D '((C 0) (D 1))))
      "(C 0) is not is set {(C 0) (D 1)}")
  (is-false     (f:belongs?-fuzzy 'E '((C 0) (D 1))) "")
  )

(test clean?-fuzzy
  "Clean a fuzzy set returning only x E A"
  (is (equalp '()      (f:clean?-fuzzy '())) "")
  (is (equalp '(A B C) (f:clean?-fuzzy '((A 1) (B 1) (C 0)))) "")
  (is (equalp '(1 2)   (f:clean?-fuzzy '((1 A) (2 B)))) "")
  )

(defvar Au '((1 0.7) (2 0.1) (3 0.3) (4 0.9) (5 0.2)))
(defvar Bu '((1 0.1) (2 0.8) (3 0.9) (4 0.2) (5 1.0)))

(defvar Ao '((35 0.1) (45 0.2) (55 0.4) (65 0.7) (75 0.9) (80 1.0)))
(defvar Ao-names '((Paul 0.1) (John 0.2) (Mary 0.4) (Klaus 0.7) (Juan 0.9)
                   (Agatha 1.0)))

(test union?-fuzzy
  "Test union of fuzzy sets"
  (is (equalp '((1 0.7) (2 0.8) (3 0.9) (4 0.9) (5 1.0))
              (f:union?-fuzzy Au Bu)) "Max of union")
  )

(test intersect?-fuzzy
  "Test intersection of fuzzy sets"
  (is (equalp '((1 0.1) (2 0.1) (3 0.3) (4 0.2) (5 0.2))
              (reverse (f:intersection?-fuzzy Au Bu))))
  )

(test complement?-fuzzy
  "Test fuzzy complements"
  (is (equalp '((35 0.9) (45 0.8) (55 0.6) (65 0.3) (75 0.100000024) (80 0.0))
              (f:complement?-fuzzy Ao)) "")
  (is (equalp '((Paul 0.9) (John 0.8) (Mary 0.6) (Klaus 0.3)
                (Juan 0.100000024) (Agatha 0.0))
              (f:complement?-fuzzy Ao-names)) "")
  )

(defvar cpA '((A 0.3) (B 0.8)))
(defvar cpB '((1 0.2) (2 0.6)))

(test cartesian-product?-fuzzy
  "Test fuzzy cartesian products"
  (is (equalp '((A 1 0.2) (A 2 0.3) (B 1 0.2) (B 2 0.6))
              (f:cartesian-product?-fuzzy cpA cpB))))

(defvar cold-temp  '(cold 0.0 0.0 10.0 20.0))
(defvar comfy-temp '(medium 10.0 20.0 30.0 40.0))
(defvar hot-temp   '(hot 30.0 40.0 100.0 100.0))

(test belongs-fuzzy
  "Test relational set membership"
  (is-true (f:belongs-fuzzy cold-temp 7.0))
  (is-true (f:belongs-fuzzy comfy-temp 23.0))
  (is-false (f:belongs-fuzzy hot-temp 12.0))
  )

(test set-member?-fuzzy
  "Test set membership"
  (is (equalp '(cold 0) (f:set-member?-fuzzy cold-temp 100.0)) "")
  (is (equalp '(medium 1) (f:set-member?-fuzzy comfy-temp 25.0)) "")
  (is (equalp '(hot 0.5)  (f:set-member?-fuzzy hot-temp 35.0)) "")
  (is (equalp '(hot 1.0)  (f:set-member?-fuzzy hot-temp 100.0)) "")
  (is (equalp '(hot 0.5)  (f:belongs2?-fuzzy hot-temp 35.0)) "")
  )

(test alpha-cut?-fuzzy
  "Test alpha cut of fuzzy sets"
  (is (equalp '(B1 9.099999 12.900001)
              (f:alpha-cut?-fuzzy '(B1 7 10 12 15) 0.7))
      "")
  (is (equalp '(B1 10 12)
              (f:alpha-cut?-fuzzy '(B1 7 10 12 15) 1.0))
      "")
  (is (equalp '(B1 7.0000005 15)
              (f:alpha-cut?-fuzzy '(B1 7 10 12 15) 0.0))
      "")
  )

(test def-set-fuzzy
  ""
  (is (equalp '(young 15 25 25 35)
              (f:def-set-fuzzy 'young '(15.0 35.0 0) '(25.0 25.0 1.0)))
      "")
  (is (equalp '(mature 35 45 55 75)
              (f:def-set-fuzzy 'mature '(35.0 75.0 0.0) '(45.0 55.0 1.0)))
      "")
  )

(defvar dA '(B1 (7 0.0) (31/4 0.25) (17/2 0.5) (37/4 0.75) (10 1.0)
             (21/2 1.0) (11 1.0) (23/2 1.0) (12 1.0) (51/4 0.75)
             (27/2 0.5) (57/4 0.25) (15 0.0)))

(test discretise?-fuzzy
  "Test FLSSR (fuzzy-set-name x1 x2 x3 x4)
  and get discrete representation"
  (is (equalp dA (f:discretise?-fuzzy '(B1 7 10 12 15) 4)) "")
  )

(test dset-membership?-fuzzy
  "Test Discrete Membership"
  (is (equalp '(B1 0.39999992) (f:dset-member?-fuzzy dA 8.2)) "")
  (is (equalp '(B1 0.39999995) (f:set-member?-fuzzy '(B1 7 10 12 15) 8.2)))
  )

(defvar dfA '(BELL (1.5 0.0d0) (1.6 0.09549154683847255d0) (1.7 0.3454916452834731d0)
 (1.8000001 0.654508710893868d0) (1.9000001 0.9045086732911676d0) (2.0 1.0d0)
 (2.1 0.9045086732911676d0) (2.1999998 0.6545090670711222d0)
 (2.2999997 0.3454923576384152d0) (2.3999996 0.09549220722875423d0)
 (2.4999995 2.244093799674829d-12)))
;;;
(defvar dfU '(U2 (1.5 0.6666667) (1.6 0.7575758) (1.7 0.84745765) (1.8000001 0.92592597)
 (1.9000001 0.98039216) (2.0 1.0) (2.1 0.98039216) (2.1999998 0.9259261)
 (2.2999997 0.84745795) (2.3999996 0.75757605) (2.4999995 0.6666671)))

(test discretise-fn-fuzzy
  "Test function discretise"
  (is (equalp dfA
              (f:discretise-fn?-fuzzy
               'Bell
               (lambda (x) (/ (+ 1.0 (cos (* 2.0 pi (- x 2.0)))) 2.0))
               10 1.5 2.5)))
  (is (equalp dfU
              (f:discretise-fn?-fuzzy
               'U2
               (lambda (x) (/ 1.0 (+ 1.0 (* 2.0 (* (- x 2.0) (- x 2.0))))))
               10 1.5 2.5)))
  )

(test complement-membership?-fuzzy
  "Test fuzzy complement"
  (is (equalp '(B1 0.3333333)
              (f:set-complement-membership?-fuzzy '(B1 7 10 12 15) 9)) "")
  (is (equalp '(B1 0.0)
              (f:set-complement-membership?-fuzzy '(B1 7 10 12 15) 11)) "")
  )

(defvar fA '(Triangle 0 5 5 10))
(defvar fB '(Trapezium 5 10 15 20))

(test union-membership?-fuzzy
  "Test fuzzy union"
  (is (equalp '(AuB 0.5) (f:set-union-membership?-fuzzy 'AuB fA fB 7.5)) "")
  )

(test intersect-membership?-fuzzy
  "Test fuzzy intersection"
  (is (equalp '(AiB 0.4) (f:set-intersect-membership?-fuzzy 'AiB fA fB 8)) "")
  )

(test intv-add
  ""
  (is (equalp '(3 7)
              (f:intv-add-fuzzy 2 4 1 3)) "")
  )

(test intv-sub
  ""
  (is (equalp '(-1 3) (f:intv-sub-fuzzy 2 4 1 3)) "")
  )

(test intv-mul
  ""
  (is (equalp '(2 12) (f:intv-mult-fuzzy 2 4 1 3)) "")
  )

(test intv-div
  ""
  (is (equalp '(2/3 4)
              (f:intv-div-fuzzy 2 4 1 3)) "")
  )

(defvar caA '(around-2 1.75 2 2 2.25))
(defvar caB '(around-5 4.8 5 5 5.2))

(test add-fuzzy
  ""
  (is (equalp '(A+B 6.55 6.9999995 6.9999995 7.4499993)
              (f:add-fuzzy 'A+B caA caB)) "")
  (is (equalp '(A-B -3.4499996 -2.9999998 -2.9999998 -2.55)
              (f:sub-fuzzy 'A-B caA caB)) "")
  )

;;
(defvar ccAM '(AXB (8.400001 0.0) (8.712 0.2) (9.027999 0.4)
               (9.348001 0.6) (9.671999 0.8) (10.0 1.0)
               (10.332 0.8) (10.667998 0.6) (11.008 0.4) (11.351999 0.2)
               (11.7 0.0)))

(test mult-fuzzy
  ""
  (is (equalp ccAM (f:mult-fuzzy 'AxB caA caB 5)) "")
  )

(defvar ccAD '(B/A (2.1333334 0.0) (2.2 0.2) (2.2697673 0.4)
               (2.3428574 0.6) (2.419512 0.8) (2.5 1.0) (2.5846152 0.8)
               (2.673684 0.6) (2.7675676 0.4) (2.8666663 0.2) (2.9714284 0.0)))

(test div-fuzzy
  ""
  (is (equalp ccAD (f:div-fuzzy 'B/A caB caA 5)) "")
  )

(test fuzzy-factor
  ""
  (is (equalp '(A1 -6 9 9 24)
              (f:fuzzy-factor '(A1 -2 3 3 8) 3)) "")
  (is (equalp '(A1 -0.5 0.75 0.75 2)
              (f:fuzzy-factor '(A1 -2 3 3 8) 0.25)) "")
  )

(test fuzzy-shift
  ""
  (is (equalp '(yp 20.0 30.0 40.0 50.0)
              (f:fuzzy-shift '(yp 15.0 25.0 35.0 45.0) 5.0)) "")
  (is (equalp '(tp 2.0 3.0 4.0 5.0)
              (f:fuzzy-shift '(tp 1.0 2.0 3.0 4.0) 1.0)) "")
  )

(test expand-contract-fuzzy
  ""
  (is (equalp '(a 4 4 4 4)  (f:expand-contract-fuzzy '(a 2 3 5 6) 0)))
  (is (equalp '(a -1 1 1 3) (f:expand-contract-fuzzy '(a 0 1 1 2) 2.0)) "")
  (is (equalp '(a 0 1 1 2)  (f:expand-contract-fuzzy '(a -1 1 1 3) 0.5)) "")
  )

(defvar F1 '(set1 -2 0 0 2))
(defvar F2 '(set2 3 5 5 7))
(defvar F3 '(set3 6 7 7 8))
(defvar F4 '(set4 7 9 11 12))
(defvar F5 '(set5 8 10 10 12))

(defvar fsets '(F1 F2 F3 F4 F5))
;; (print (f:list-sets-fuzzy fsets))
;; (print (f:lv-membership-fuzzy fsets 10))
;; (print (f:lv-membership?-fuzzy fsets 10))

(test add-sets-fuzzy
  ""
  (is (equalp '(SOF 22.0 29.857138 31.85714 38.0)
              (f:add-sets-fuzzy fsets 'SOF)) "")
  )

;; (print (f:add-sets-fuzzy fsets 'SOF))

(test avg-fuzzy
  ""
  (is (equalp '(AVG 4.4 5.9714274 6.371428 7.6)
              (f:avg-fuzzy fsets 'AVG))))

(test simple-defuzzification
  ""
  (is (equalp 2     (f:simple-defuzzification '(q 0 1 1 5) 1)))
  (is (equalp 1.375 (f:simple-defuzzification '(q 0 1 1 5) 4)))
  )

(defvar age1 '(young   0  0  15 30))
(defvar age2 '(young+  15 30 30 45))
(defvar age3 '(mature  30 45 45 60))
(defvar age4 '(mature+ 45 60 60 75))
(defvar age5 '(old     60 75 90 90))

(defvar lv-age '(age1 age2 age3 age4 age5))
;; (print (f:list-sets-fuzzy lv-age))
;; (print (f:lv-membership-fuzzy lv-age 32))
;; (print (f:lv-membership?-fuzzy lv-age 32))

(defvar agge1 '(young   0  0  0  90))
(defvar agge2 '(young+  0  30 30 90))
(defvar agge3 '(mature  0  45 45 90))
(defvar agge4 '(mature+ 0  60 60 90))
(defvar agge5 '(old     0  90 90 90))

(defvar lv-agge '(agge1 agge2 agge3 agge4 agge5))
;; (print (f:list-sets-fuzzy lv-agge))
;; (print (f:lv-membership-fuzzy lv-agge 23))
;; (print (f:lv-membership?-fuzzy lv-agge 23))

(defvar ff1 #'(lambda (x) (/ (+ 1.0 (cos (* 0.0333 pi (- x 0.0))))  2.0)))
(defvar ff2 #'(lambda (x) (/ (+ 1.0 (cos (* 0.067  pi (- x 30.0)))) 2.0)))
(defvar ff3 #'(lambda (x) (/ (+ 1.0 (cos (* 0.067  pi (- x 45.0)))) 2.0)))
(defvar ff4 #'(lambda (x) (/ (+ 1.0 (cos (* 0.067  pi (- x 60.0)))) 2.0)))
(defvar ff5 #'(lambda (x) (/ (+ 1.0 (cos (* 0.0333 pi (- x 90.0)))) 2.0)))
;;
(defvar dBell1 (discretise-fn?-fuzzy 'young   ff1 20  0 30))
(defvar dBell2 (discretise-fn?-fuzzy 'young+  ff2 20 15 45))
(defvar dBell3 (discretise-fn?-fuzzy 'mature  ff3 20 30 60))
(defvar dBell4 (discretise-fn?-fuzzy 'mature+ ff4 20 45 75))
(defvar dBell5 (discretise-fn?-fuzzy 'old     ff5 20 60 90))
;;
(defvar lv-age-bells '(dBell1 dbell2 dbell3 dbell4 dbell5))
;;
;; (print (f:dlv-membership?-fuzzy lv-age-bells 23))
;;
(run! 'fuzzy-sets)

(def-suite frbs)

(def-suite frbs-classical-logic :in frbs)
(in-suite frbs-classical-logic)

(test classical-logic
  "Test all classical logic operations
  T = 1
  F = 0

  p ^ q  = min(p,q)
  p v q  = max(p,q
  !p     = 1 - p
  p -> q = min(1, 1 + q - p)
"
  (is-true  (and t t))
  (is-true  (or t t))
  (is-false (not t))
  (is-true  (f:implication-frbs t t))
  ;;
  (is-false (and t nil))
  (is-true  (or t nil))
  (is-false (not t))
  (is-false (f:implication-frbs t nil))
  ;;
  (is-false (and nil t))
  (is-true  (or nil t))
  (is-true  t)
  (is-true  (f:implication-frbs nil t))
  ;;
  (is-false (and nil nil))
  (is-false (or nil nil))
  (is-true  (not nil))
  (is-true  (f:implication-frbs nil nil))
  )
;;
;;
(defvar aP '(young 0 0 15 30))
(defvar aQ '(old   50 90 90 90))
;;
(defvar adP (f:discretise?-fuzzy '(young 0 0 15 30) 4))
(defvar adQ (f:discretise?-fuzzy '(old   50 90 90 90) 4))
;;
(def-suite frbs-fuzzy-logic :in frbs)
(in-suite frbs-fuzzy-logic)
;;
(test p-and-q
  ""
  (is (equalp 0.125 (f:p-and-q?-fuzzy aQ aP 55 18)))
  )

(test p-or-q
  ""
  (is (equalp 0.8 (f:p-or-q?-fuzzy aQ aP 55 18)))
  )

(test p-not
  ""
  (is (equalp 0.875      (f:p-not?-fuzzy aQ 55)))
  (is (equalp 0.19999999 (f:p-not?-fuzzy aP 18)))
  )

(test p-implies-q
  ""
  (is (equalp 1.0 (f:p-implies-q?-fuzzy aQ aP 55 10)))
  (is (equalp 0.0 (f:p-implies-q?-fuzzy aQ aP 90 30)))
  )

(test dp-and-q
  ""
  (is (equalp 0.125 (f:dp-and-q?-fuzzy adQ adP 55 18)))
  )

(test dp-or-q
  ""
  (is (equalp 0.8 (f:dp-or-q?-fuzzy adQ adP 55 18)))
  )

(test dp-not
  ""
  (is (equalp 0.875      (f:dp-not?-fuzzy adQ 55)))
  (is (equalp 0.19999999 (f:dp-not?-fuzzy adP 18)))
  )

(test dp-implies-q
  ""
  (is (equalp 1.0 (f:dp-implies-q?-fuzzy adQ adP 55 10)))
  (is (equalp 0.0 (f:dp-implies-q?-fuzzy adQ adP 90 30)))
  )

(defvar hA '(A1 (7 0.0) (31/4 0.5) (17/2 0.70710677) (37/4 0.8660254) (10 1.0) (21/2 1.0)
 (11 1.0) (23/2 1.0) (12 1.0) (51/4 0.8660254) (27/2 0.70710677) (57/4 0.5)
 (15 0.0)))

(defvar hB '(A1 (7 0.0) (31/4 0.0625) (17/2 0.25) (37/4 0.5625) (10 1.0) (21/2 1.0)
 (11 1.0) (23/2 1.0) (12 1.0) (51/4 0.5625) (27/2 0.25) (57/4 0.0625) (15 0.0)))

(test dset-hedge?-fuzzy
  ""
  (is (equalp hA (f:dset-hedge?-fuzzy (f:discretise?-fuzzy '(A1 7 10 12 15) 4) "FAIRLY")))
  (is (equalp hB (f:dset-hedge?-fuzzy (f:discretise?-fuzzy '(A1 7 10 12 15) 4) "VERY")))
  )

(run! 'frbs)

;;;
;;;
;;; Test the use of fuzzy sets in control simulations
;;;
;;;
(def-suite control)
;;;
;;;
(run-all-tests)
