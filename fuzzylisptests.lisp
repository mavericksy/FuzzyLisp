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
  (is (equalp cU (sort (union cA (f:setcomplement? cA cU)) #'<))        "A u A' != U")
  (is (equalp cE (sort (intersection cA (f:setcomplement? cA cU)) #'<)) "A n A' != 0")
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
  (is (equalp '(1 2 3 4 5 67) (f:setcomplement? A U)) "Set complement must be '(1 2 3 4 5 67)")
  (is (equalp '()             (f:setcomplement? '() '())) "Empty setcomplement is empty set")
  )

(test union-classical
  "Test the union properties of sets"
  (is (equalp '()                      (union '() '())) "Union of empty sets is empty set")
  (is (equal '(67 5 4 3 2 1 42 70 100) (union A U)) "Union A with U must be U")
  (is (equalp A                        (union A A)) "Union A must be A")
  (is (equalp '(42 70 100 43 71 101)   (union (reverse A) B)) "Union must be '(42 70 100 43 71 101)")
  (is (equalp A                        (union A '())) "Union with empty set must be A")
  )

(test intersect-classical
  "Test the intersection properties of sets"
  (is (equalp '()          (intersection A '())) "Intersection should be empty set")
  (is (equalp '(42 70 100) (intersection (reverse A) A)) "A intersection with itself should be A")
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
  (is (equalp '()                        (f:cartesian-product? '() '())) "Products of empty sets must be an empty set")
  (is (equalp '((D 1) (D 2) (E 1) (E 2)) (f:cartesian-product? DE FG)))
  (is (equalp '((1 D) (1 E) (2 D) (2 E)) (f:cartesian-product? FG DE)))
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
  (is-false     (f:belongs?-fuzzy 'A '((C 0) (D 1)))  "(A 1) is not is set {(C 0) (D 1)}")
  (is (equalp 0 (f:belongs?-fuzzy 'C '((C 0) (D 1)))) "(C 0) is not is set {(C 0) (D 1)}")
  (is (equalp 1 (f:belongs?-fuzzy 'D '((C 0) (D 1)))) "(C 0) is not is set {(C 0) (D 1)}")
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
(defvar Ao-names '((Paul 0.1) (John 0.2) (Mary 0.4) (Klaus 0.7) (Juan 0.9) (Agatha 1.0)))

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
  (is (equalp '((Paul 0.9) (John 0.8) (Mary 0.6) (Klaus 0.3) (Juan 0.100000024) (Agatha 0.0))
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
  (is (equalp '(medium 1) (f:set-member?-fuzzy comfy-temp 25)) "")
  (is (equalp '(hot 0.5)  (f:set-member?-fuzzy hot-temp 35)) "")
  (is (equalp '(hot 0.5)  (f:belongs2?-fuzzy hot-temp 35)) "")
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

(defvar dA '(B1 (7 0.0) (31/4 0.25) (17/2 0.5) (37/4 0.75) (10 1.0) (21/2 1.0) (11 1.0)
 (23/2 1.0) (12 1.0) (51/4 0.75) (27/2 0.5) (57/4 0.25) (15 0.0)))

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

(test discretise-fn-fuzzy
  "Test function discretise"
  (is (equalp dfA
              (f:discretise-fn-fuzzy
               'Bell
               (lambda (x) (/ (+ 1.0 (cos (* 2.0 pi (- x 2.0)))) 2.0))
               10 1.5 2.5)))
  )

(print (f:discretise-fn-fuzzy
               'U2
               (lambda (x) (/ 1.0 (+ 1.0 (* 2.0 (* (- x 2.0) (- x 2.0))))))
               10 1.5 2.5))

;;
(run! 'fuzzy-sets)
;;;
;;;
;;; Test the use of fuzzy sets in control simulations
;;;
;;;
(def-suite control)
;;;
;;;
(run-all-tests)
