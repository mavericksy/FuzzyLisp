;;
;; Fuzzy Logic
;;
(in-package :cl-user)
(defpackage #:fuzzylogic
  (:use :cl)
  (:export
   :prinn
   :belongs
   :subset
   :cardinality
   :equivalent
   :eql-c
   :disjoint
   :setcomplement?
   :cartesian-product?
   :union-cs?
   ;;
   :belongs-fuzzy
   :belongs?-fuzzy
   :belongs2?-fuzzy
   ;;
   :clean?-fuzzy
   :union?-fuzzy
   :intersection?-fuzzy
   :complement?-fuzzy
   :cartesian-product?-fuzzy
   :set-member?-fuzzy
   ;;
   :alpha-cut?-fuzzy
   ;;
   :def-set-fuzzy
   :discretise?-fuzzy
   :dset-member?-fuzzy
   :discretise-fn?-fuzzy
   ;;
   :set-complement-membership?-fuzzy
   :set-union-membership?-fuzzy
   :set-intersect-membership?-fuzzy
   ;;
   :intv-add-fuzzy
   :intv-sub-fuzzy
   :intv-mult-fuzzy
   :intv-div-fuzzy
   ;;
   :add-fuzzy
   :sub-fuzzy
   :mult-fuzzy
   :div-fuzzy
   ;;
   :fuzzy-factor
   :fuzzy-shift
   :expand-contract-fuzzy
   ;;
   :add-sets-fuzzy
   :avg-fuzzy
   ;;
   :simple-defuzzification
   ;;
   :list-sets-fuzzy
   :lv-membership-fuzzy
   :lv-membership?-fuzzy
   ;;
   :dlv-membership?-fuzzy
   ;;
   :implication-frbs
   ;;
   :p-and-q?-fuzzy
   :p-or-q?-fuzzy
   :p-not?-fuzzy
   :p-implies-q?-fuzzy
   ;;
   :dp-and-q?-fuzzy
   :dp-or-q?-fuzzy
   :dp-not?-fuzzy
   :dp-implies-q?-fuzzy
   ;;
   :dset-hedge?-fuzzy
   ))
;;
;;
(in-package #:fuzzylogic)
;;
;;
(defun prinn (a) 
  (format t "~%~A~%" a))
;;
(defun mklist (obj)
  (if (listp obj) obj (list obj)))

;;
;;
;; Crisp Sets
;;
;;

(defun union-cs? (a b &key (pred #'<))
  "Union of classic sets A B with sort pred"
  (sort (union a b) pred))
;;
(defun belongs (x A)
  "x belongs A"
  (if (or (intersection (mklist x) (mklist A))
          (equalp x '()))
      t nil))
;;
(defun subset (a b)
  "A subset B"
  (if (or (equalp (reverse (mklist a))
                 (intersection (mklist a) (mklist b)))
          (equalp a '()))
      t nil))
;;
(defun cardinality (a)
  "Countable set have a number of elements"
  (length a))
;;
(defun equivalent (a b)
  "Equivalent sets have the same cardinality"
  (if (equalp (cardinality a) (cardinality b))
      t nil))
;;
(defun eql-c (a b)
  (equalp a b))
;;
(defun disjoint (a b)
  (if (equalp (intersection (mklist a) (mklist b)) '())
      t nil))
;;
(defun setcomplement? (a u)
  "Return the complementary of A with respect to U"
  (let ((out '())
        (len (cardinality u)))
    (loop for i from 0 below len
          do (if (not (belongs (nth i u) a))
                 (setq out (cons (nth i u) out))))
    (reverse out)))
;;
(defun cartesian-product? (a b)
  "A x B = {(x,y) | x E A and y E B}"
  (let ((la (cardinality a))
        (lb (cardinality b))
        (out '()))
   (loop for i from 0 below la
        do (loop for j from 0 below lb
               do (setq out (cons (list (nth i a) (nth j b)) 
                                  out))))
    (reverse out)))
;;
;;
;; Sorites Paradox and Fuzzy Sets
;;
;; A fuzzy set A is defined by a characteristic function Ua
;; that maps every elemenet x E A to the closed interval of
;; reals [0,1]
;;
;; A = {(x,Ua(x)) | x E A, Ua(x) E [0,1] }
;; Ua : X -> [0,1]
;;
;; Ua(x) = 1 for x  E A
;; Ua(x) = 0 for x !E A
;;

(defun clean?-fuzzy (a)
  "
    C = Uc(x) = first[x, Ua(x)]
"
  (let ((la (cardinality a))
        (out '()))
    (loop for i from 0 below la
          do (setf out (cons (first (nth i a)) out)))
    (reverse out)))
;;
;;
(defun union?-fuzzy (a b)
  "
    C = A u B = Uc(x) = max[Ua(x), Ub(x)]
"
  (let* ((temp (union (clean?-fuzzy a) (clean?-fuzzy b)))
         (out '()))
    (loop for i from 0 below (cardinality temp)
          do (let* ((el (nth i temp))
                    (mdA (assoc el a))
                    (mdB (assoc el b)))
               (if (>= (second mdA) (second mdB))
                   (setf out (cons mdA out))
                   (setf out (cons mdB out)))))
    (reverse out)))
;;
(defun intersection?-fuzzy (a b)
  "
    C = A n B = Uc(x) = min[Ua(x), Ub(x)]
"
  (let* ((temp (intersection (clean?-fuzzy a) (clean?-fuzzy b)))
         (out '()))
    (loop for i from 0 below (cardinality temp)
          do (let* ((el (nth i temp))
                    (mdA (assoc el a))
                    (mdB (assoc el b)))
               (if (<= (second mdA) (second mdB))
                   (setf out (cons mdA out))
                   (setf out (cons mdB out)))))
    (reverse out)))
;;
(defun complement?-fuzzy (a)
  "
    A' = 1 - Ua(x)
"
  (let ((lA (cardinality a))
        (out '()))
    (loop for i from 0 below lA
          do (let* ((el (nth i a)))
               (setf out (cons (list (first el)
                                     (- 1.0 (car (last el))))
                               out))))
    (reverse out)))
;;
(defun cartesian-product?-fuzzy (a b)
  "
    A x B = {(x,y, Ur(x,y)) | x E A and y E B}
    Ur(x,y) = min(Ua(x), Ub(y))
"
  (let ((lA (cardinality a))
        (lB (cardinality b))
        (out '()))
    (loop for i from 0 below lA
          do (loop for j from 0 below lB
                   do (setf out (cons 
                                 (list (first (nth i a))
                                       (first (nth j b))
                                       (min (cadr (nth i a))
                                            (cadr (nth j b))))
                                 out))))
    (reverse out)))
;;
;;
(defun belongs?-fuzzy (x A)
  ""
  (if (assoc x A)
      (car (last (assoc x A)))
      nil))
;;
(defun belongs-fuzzy (set x)
  ""
  (if (and (>= x (nth 1 set)) (<= x (nth 4 set)))
      t
      nil))
;;
;;
;; Support, Nucleus, Alpha Cut
;; s  = x / x E [x1, x4]
;; k  = x / Y x E [x2, x3], f(x) = 1.0
;; aA = x / f(x) >= a
;;
;;
(defun set-member?-fuzzy (set x)
  (let ((name (nth 0 set))
        (x1 (nth 1 set))                ; support
        (x2 (nth 2 set))                ; nucleus starts
        (x3 (nth 3 set))                ; nuclues ends
        (x4 (nth 4 set))
        (mem-deg nil))                  ; support ends
    (cond
      ((or (< x x1) (> x x4))         ; degree 0.0
       (setf mem-deg 0.0))
      ((and (>= x x1) (< x x2))          ; increasing degree
       (setf mem-deg (/ (* (- x x1) 1.0)
                        (- x2 x1))))
      ((and (>= x x2) (<= x x3))        ; nucleus is always 1.0
       (setf mem-deg 1.0))
      ((and (> x x3) (<= x x4))          ; decreasing degree
       (setf mem-deg (/ (* (- x4 x) 1.0)
                        (- x4 x3)))))
    (list name mem-deg)))
;;
(defun belongs2?-fuzzy (set x)
  (if (and (>= x (nth 1 set)) (<= x (nth 4 set)))
      (set-member?-fuzzy set x)
      nil))
;;
(defun alpha-cut?-fuzzy (set alpha)
  (let ((name (nth 0 set))
        (x1 (nth 1 set))
        (x2 (nth 2 set))
        (x3 (nth 3 set))
        (x4 (nth 4 set))
        (ex_l 0.0)
        (ex_r 0.0))
    (if (= x1 x2)
        (setf ex_l x1))
    (if (/= x1 x2)
        (let* ((tan_phi1 (/ 1.0 (- x2 x1)))
               (frac (* tan_phi1 x2))
               (nume (- (+ frac alpha) 1.0)))
          (setf ex_l (/ nume tan_phi1))))
    (if (= x3 x4)
        (setf ex_l x4))
    (if (/= x3 x4)
        (let* ((tan_phi1 (/ 1.0 (- x4 x3)))
               (frac (* tan_phi1 x3))
               (nume (- (+ frac 1.0) alpha)))
          (setf ex_r (/ nume tan_phi1))))
    (list name ex_l ex_r)))
;;
(defun def-set-fuzzy (name acut1 acut2)
  "Return set A defined by 2 alpha cuts as a
    a1 < a2
    acut:
    (a1 a2 deg)

    returns:
    Fuzzy Logic Standard Set Representation
    FLSSR (name x1 x2 x3 x4) "

  (let* ((triangle nil)
         (x1a (nth 0 acut1))
         (x1b (nth 1 acut1))
         (alpha1 (nth 2 acut1))
         (x2a (nth 0 acut2))
         (x2b (nth 1 acut2))
         (alpha2 (nth 2 acut2))
         (m1 0.0)
         (m2 0.0)
         (base1 0.0)
         (base2 0.0)
         (base3 0.0)
         (base4 0.0)
         (tempx 0.0)
         (tempy 0.0))
    (if (< (abs (- x1a x2a)) 0.000001)
        ;; slope tangent equals inf
        (setf m1 sb-ext:single-float-positive-infinity)
        (setf m1 (aux-calculate-m-fuzzy x1a x2a alpha1 alpha2)))
    (if (< (abs (- x1b x2b)) 0.000001)
        (setf m2 sb-ext:single-float-positive-infinity)
        (setf m2 (aux-calculate-m-fuzzy x1b x2b alpha1 alpha2)))
    ;; calculate x axis intersections
    ;; base1 and base4 are extremes support
    (setf base1 (- x1a (/ alpha1 m1)))
    (if (< m2 0.0)
        (setf m2 (* m2 -1.0)))
    (setf base4 (+ (/ alpha1 m2) x1b))
    ;; base2 and base3 represent the nucleus
    (setf base2 (/ (+ 1 (* m1 base1)) m1))
    (setf base3 (/ (- (* m2 base4) 1.0) m2))
    ;; check triangular membership function
    (if (>= base2 base3)
        (progn
          (setf triangle t
                tempx (/ (+ (* m1 base1) (* m2 base4))
                         (+ m1 m2)))
          (setf tempy (* m1 (- tempx base1)))))
    (if triangle
        (list name base1 tempx tempx base4)
        (list name base1 base2 base3 base4))))
;;
(defun aux-calculate-m-fuzzy (x1 x2 y1 y2)
  (/ (- y2 y1) (- x2 x1)))
;;
;;
(defun discretise?-fuzzy (set steps)
  "
    set:
    Fuzzy Logic Standard Set Representation
    FLSSR (name x1 x2 x3 x4)

    returns:
    Fuzzy Logic Discrete Set Representation
    FLDSR (name x1U(x1) x2U(x2)...xNU(xN) )
"
  (let* ((name (nth 0 set))
         (x1 (nth 1 set))
         (x2 (nth 2 set))
         (x3 (nth 3 set))
         (x4 (nth 4 set))
         (out (list name))
         (trapezium t)
         (resolution (/ (- x2 x1) steps))
         (x x1))
    (loop for i from 0 below steps
          do (progn
               (setf out (cons (list x (car (last (set-member?-fuzzy set x))))
                               out))
               (setf x (+ x resolution))))
    (if (< (- x3 x2) 0.0000001)
        (progn
          (setf out (cons (list x2 1.0) out))
          (setf trapezium nil))
        (progn
          (setf resolution (/ (- x3 x2) steps))
          (setf x x2)
          (loop for i from 0 below steps
                do (progn
                     (setf out (cons (list x 1.0) out))
                     (setf x (+ x resolution))))))
    (setf resolution (/ (- x4 x3) steps))
    (if trapezium
        (setf x x3)
        (setf x (+ x3 resolution)))     ; is triangle
    (loop for i from 0 below steps
          do (progn
               (setf out (cons (list
                                x
                                (car (last (set-member?-fuzzy set x))))
                               out))
               (setf x (+ x resolution))))
    (when trapezium
      (setf out (cons (list x 0.0) out)))
    (reverse out)))
;;
;;
(defun dset-member?-fuzzy (dfset x)
  (let ((result (list (first dfset) 0.0))
        (n (length dfset)))
    (loop for i from 1 below (- n 1)
          do (progn
               (let ((paira (nth i dfset))
                     (pairb (nth (+ 1 i) dfset)))
                 (when (and (<= (first paira) x)
                            (>= (first pairb) x))
                   (setf result (list
                                 (first dfset)
                                 (interpolation paira pairb
                                                (- x (first paira)))))))))
    result))
;;
(defun interpolation (pa pb p)
  (let* ((a (first pa))
        (b (first pb))
        (c (if (= 0 (- b a)) 1 (- b a)))
        (y1 (car (last pa)))
        (y2 (car (last pb))))
    ;; analytic geometry
    (+ y1 (/ (* p (- y2 y1)) c))))
;;
;;
;;
(defun discretise-fn?-fuzzy (name fn steps a b)
  (let ((out (list name))
        (resolution (/ (- b a) steps)))
    (loop with x = a while (<= x (+ b 0.00001))
          do (progn
               (setf out (cons (list x (funcall fn x)) out))
               (setf x (+ x resolution))))
    (reverse out)))
;;
(defun set-complement-membership?-fuzzy (set x)
  "
    A' = 1 - Ua(x) / x E [x1, x4]
    A' = 1 Y x !E [x1, x4], x E U
"
  (list (first (set-member?-fuzzy set x))
        (- 1.0 (car (last (set-member?-fuzzy set x))))))
;;
(defun set-union-membership?-fuzzy (name set1 set2 x)
  "
    A u B = max(Ua(x), Ub(x)) / x E [x1A, x4B]
"
  (let ((mu1 (car (last (set-member?-fuzzy set1 x))))
        (mu2 (car (last (set-member?-fuzzy set2 x)))))
    (list name (max mu1 mu2))))
;;
(defun set-intersect-membership?-fuzzy (name set1 set2 x)
  "
    A n B = min(Ua(x), Ub(x)) / x E [x1A, x4B]
"
  (let ((mu1 (car (last (set-member?-fuzzy set1 x))))
        (mu2 (car (last (set-member?-fuzzy set2 x)))))
    (list name (min mu1 mu2))))
;;
;;
;;
(defun intv-add-fuzzy (x1 x2 x3 x4)
  (list (+ x1 x3) (+ x2 x4)))
;;
(defun intv-sub-fuzzy (x1 x2 x3 x4)
  (list (- x1 x4) (- x2 x3)))
;;
(defun intv-mult-fuzzy (x1 x2 x3 x4)
  (let* ((ex-l (min (* x1 x3)  (* x1 x4) (* x2 x3) (* x2 x4)))
         (ex-r (max (* x1 x3)  (* x1 x4) (* x2 x3) (* x2 x4))))
    (list ex-l ex-r)))
;;
(defun intv-div-fuzzy (a b d e)
  (let* ((ex-l (min (/ a d) (/ a e) (/ b d) (/ b e)))
         (ex-r (max (/ a d) (/ a e) (/ b d) (/ b e))))
    (list ex-l ex-r)))
;;
;; FIXME better macro expansion without crowbaring progn
(defmacro multi-pop (&rest body)
  (loop for l in body
        collect `(pop ,l) into r
        finally (return (cons 'progn r))))
;;
(defun add-fuzzy (name a b)
  (let ((cut1a (alpha-cut?-fuzzy a 0.25))
        (cut1b (alpha-cut?-fuzzy b 0.25))
        (cut2a (alpha-cut?-fuzzy a 0.75))
        (cut2b (alpha-cut?-fuzzy b 0.75)))
    (multi-pop cut1a cut1b cut2a cut2b)
    (let ((sum1 (intv-add-fuzzy (nth 0 cut1a) (nth 1 cut1a) 
                                (nth 0 cut1b) (nth 1 cut1b)))
          (sum2 (intv-add-fuzzy (nth 0 cut2a) (nth 1 cut2a)
                                (nth 0 cut2b) (nth 1 cut2b))))
      (setf sum1 (reverse sum1)
            sum2 (reverse sum2))
      (setf sum1 (push '0.25 sum1))
      (setf sum2 (push '0.75 sum2))
      (def-set-fuzzy name (reverse sum1) (reverse sum2)))))
;;
(defun sub-fuzzy (name a b)
  (let ((cut1a (alpha-cut?-fuzzy a 0.25))
        (cut1b (alpha-cut?-fuzzy b 0.25))
        (cut2a (alpha-cut?-fuzzy a 0.75))
        (cut2b (alpha-cut?-fuzzy b 0.75)))
    (multi-pop cut1a cut1b cut2a cut2b)
    (let ((sum1 (intv-sub-fuzzy (nth 0 cut1a) (nth 1 cut1a)
                                (nth 0 cut1b) (nth 1 cut1b)))
          (sum2 (intv-sub-fuzzy (nth 0 cut2a) (nth 1 cut2a)
                                (nth 0 cut2b) (nth 1 cut2b))))
      (setf sum1 (reverse sum1)
            sum2 (reverse sum2))
      (setf sum1 (push '0.25 sum1))
      (setf sum2 (push '0.75 sum2))
      (def-set-fuzzy name (reverse sum1) (reverse sum2)))))
;;
;;
(defun mult-fuzzy (name a b n)
  (let ((head '())
        (tail '())
        (interval (/ 1.0 n))
        (alpha 0.0))
    (loop for i from 0 upto n
          do (let* ((cutA (rest (alpha-cut?-fuzzy a alpha)))
                    (cutB (rest (alpha-cut?-fuzzy b alpha)))
                    (mult (intv-mult-fuzzy (first cutA) (car (last cutA)) (first cutB) (car (last cutB)))))
               (setf head (cons (append (list (first mult)) 
                                        (list alpha)) 
                                head))
               (setf tail (cons (append (list (car (last mult))) 
                                        (list alpha))
                                tail))
               (setf alpha (+ interval alpha))))
    (append (list name) (reverse head) (rest tail))))
;;
(defun div-fuzzy (name a b n)
  (let ((head '())
        (tail '())
        (interval (/ 1.0 n))
        (alpha 0.0))
    (loop for i from 0 upto n
          do (let* ((cutA (rest (alpha-cut?-fuzzy a alpha)))
                    (cutB (rest (alpha-cut?-fuzzy b alpha)))
                    (div (intv-div-fuzzy 
                          (first cutA) (car (last cutA))
                          (first cutB) (car (last cutB)))))
               (setf head (cons (append (list (first div)) 
                                        (list alpha)) 
                                head))
               (setf tail (cons (append (list (car (last div))) 
                                        (list alpha))
                                tail))
               (setf alpha (+ interval alpha))))
    (append (list name) (reverse head) (rest tail))))
;;
;;
;;
(defun fuzzy-factor (set k)
  (let ((x1 (* k (nth 1 set)))
        (x2 (* k (nth 2 set)))
        (x3 (* k (nth 3 set)))
        (x4 (* k (nth 4 set))))
    (if (>= x4 x1)
        (list (nth 0 set) x1 x2 x3 x4)
        (list (nth 0 set) x4 x3 x2 x1))))
;;
(defun fuzzy-shift (set x)
  (list (nth 0 set)
        (+ x (nth 1 set))
        (+ x (nth 2 set))
        (+ x (nth 3 set))
        (+ x (nth 4 set))))
;;
(defun expand-contract-fuzzy (set k)
  (let* ((center1 (/ (+ (nth 2 set) (nth 3 set)) 2.0))
         (result  (fuzzy-factor set k))
         (center2 (/ (+ (nth 2 result) (nth 3 result)) 2.0)))
    (fuzzy-shift result (* (- center2 center1) -1.0))))
;;
;;
;;
(defun add-sets-fuzzy (sets name)
  (let ((n (length sets))
        (out '()))
    (setf out (add-fuzzy name
                         (eval (nth 0 sets))
                         (eval (nth 1 sets))))
    (loop for i from 2 below n
          do (setf out (add-fuzzy name out (eval (nth i sets)))))
    out))
;;
(defun avg-fuzzy (sets name)
  (fuzzy-factor (add-sets-fuzzy sets name)
                (/ 1.0 (length sets))))
;;
;;
;;
(defun simple-defuzzification (set mode)
  (let ((m (/ (+ (nth 2 set)
                 (nth 3 set)) 2.0)))
    (case mode
      (1 (/ (+ (nth 1 set) m (nth 4 set)) 3.0))
      (2 (/ (+ (nth 1 set) (* m 2.0) (nth 4 set)) 4.0))
      (3 (/ (+ (nth 1 set) (* m 4.0) (nth 4 set)) 6.0))
      (4 (/ (+ (nth 1 set) (* m 6.0) (nth 4 set)) 8.0)))))
;;
(defun list-sets-fuzzy (sets)
  (loop for i from 0 below (length sets)
        do (progn (format t "~A~%" (eval (nth i sets))))))
;;
;;
(defun lv-membership-fuzzy (lv x)
  (loop for i from 0 below (length lv)
        do (let ((fset (eval (nth i lv))))
             (format t "~A~%" (set-member?-fuzzy fset x)))))
;;
(defun lv-membership?-fuzzy (lv x)
  (let ((out '()))
    (loop for i from 0 below (length lv)
          do (let ((fset (eval (nth i lv))))
               (setf out (append out (list (set-member?-fuzzy fset x))))))
    out))
;;
;;
(defun dlv-membership?-fuzzy (dlv x)
  (let ((out '()))
    (loop for i from 0 below (length dlv)
          do (let ((fset (eval (nth i dlv))))
               (setf out (append out (list (dset-member?-fuzzy fset x))))))
    out))
;;
;;
;; FRBS
;; Fuzzy Rule Based System
;;
;;
(defun implication-frbs (p q)
  (not (and p (not q))))
;;
;;
(defun truth-val?-fuzzy (set x)
  (set-member?-fuzzy set x))
;;
;;
;;
(defun p-and-q?-fuzzy (p q x y)
  "
    Tv(p^q) = min(Tv(p), Tv(q)) = min(Ua(x), Ub(y))
"
  (let ((a (car (last (set-member?-fuzzy p x))))
        (b (car (last (set-member?-fuzzy q y)))))
    (min a b)))
;;
(defun p-or-q?-fuzzy (p q x y)
  "
    Tv(p v q) = max(Tv(p), Tv(q)) = max(Ua(x), Ub(y))
"
  (let ((a (car (last (set-member?-fuzzy p x))))
        (b (car (last (set-member?-fuzzy q y)))))
    (max a b)))
;;
(defun p-not?-fuzzy (p x)
  "
    Tv(!p) = 1 - Tv(p) = 1 - (Ua(x))
"
  (- 1.0 (car (last (set-member?-fuzzy p x)))))
;;
(defun p-implies-q?-fuzzy (p q x y)
  "
    Tv(p -> q) = min(1, 1 + Tv(q) - Tv(p)) = min(1, 1 + Ub(y) - Ua(x))
"
  (let ((a (car (last (set-member?-fuzzy p x))))
        (b (car (last (set-member?-fuzzy q y)))))
    (min 1.0 (- (+ 1.0 b) a))))
;;
;;
;;
(defun dp-and-q?-fuzzy (dp dq x y)
  "
    Tv(p^q) = min(Tv(p), Tv(q)) = min(Ua(x), Ub(y))
"
  (let ((a (car (last (dset-member?-fuzzy dp x))))
        (b (car (last (dset-member?-fuzzy dq y)))))
    (min a b)))
;;
(defun dp-or-q?-fuzzy (dp dq x y)
  "
    Tv(p v q) = max(Tv(p), Tv(q)) = max(Ua(x), Ub(y))
"
  (let ((a (car (last (dset-member?-fuzzy dp x))))
        (b (car (last (dset-member?-fuzzy dq y)))))
    (max a b)))
;;
(defun dp-not?-fuzzy (dp x)
  "
    Tv(!p) = 1 - Tv(p) = 1 - (Ua(x))
"
  (- 1.0 (car (last (dset-member?-fuzzy dp x)))))
;;
(defun dp-implies-q?-fuzzy (dp dq x y)
  "
    Tv(p -> q) = min(1, 1 + Tv(q) - Tv(p)) = min(1, 1 + Ub(y) - Ua(x))
"
  (let ((a (car (last (dset-member?-fuzzy dp x))))
        (b (car (last (dset-member?-fuzzy dq y)))))
    (min 1.0 (- (+ 1.0 b) a))))
;;
;;
;;
(defun dset-hedge?-fuzzy (dset hedge)
  "
    HA = H(Ua(x))
    H1 = H1(Ua(x)) = (Ua(x))^2
    H2 = H2(Ua(x)) = (Ua(x))^1/2
"
  (let ((out (list (first dset))))
    (loop for i from 1 below (length dset)
          do (let ((sub (nth i dset)))
               (cond
                 ((equal "FAIRLY" hedge)
                  (setf out (cons (list (first sub)
                                        (sqrt (car (last sub))))
                                  out)))
                 ((equal "VERY" hedge)
                  (setf out (cons (list (first sub)
                                        (expt (car (last sub)) 2))
                                  out))))))
    (reverse out)))
