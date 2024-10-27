;;
;; Fuzzy Lisp
;;
(in-package :cl-user)
(defpackage #:fuzzywuzzy
  (:use :cl))
;;
(defun prinn (a) 
  (format t "~%~A~%" a))
;;
(defun mklist (obj)
  (if (listp obj) obj (list obj)))
;;
;; Crisp Sets
;;
(defun belongs? (x a)
  (if (or (intersection (mklist x) (mklist a))
          (equal x '()))
      t nil))
;;
(defun subset? (a b)
  (if (or (equal (mklist a) 
                 (intersection (mklist a) (mklist b)))
          (equal a '()))
      t nil))
;;
(defun cardinality (a)
  (length a))
;;
(defun equivalent (a b)
  (if (equal (cardinality a) (cardinality b))
      t nil))
;;
(defun eql? (a b)
  (equal a b))
;;
(defun disjoint? (a b)
  (if (equal (intersection (mklist a) (mklist b)) '())
      t nil))
;;
(defun setcomplement (a u)
  (let ((out '())
        (len (cardinality u)))
    (loop for i from 0 below len
          do (if (not (belongs? (nth i u) a))
                 (setq out (cons (nth i u) out))))
    (reverse out)))
;;
(defun cartesian-product (a b)
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
(defvar aset '(A B C D E F G))
(defvar U '(0 1 2 3 4 5 6 7 8 9))
(defvar A '(1 3 5 7))
(defvar B '(5 6 7 8 9))
(defvar C '(0 1 2 3 4))
;;
(prinn (belongs? aset 'A))
(prinn (belongs? aset 'Z))
(prinn (belongs? aset '()))

(prinn (subset? '(A) aset))
(prinn (subset? '(W) aset))
(prinn (subset? '() aset))

(prinn (disjoint? '(A) '(A)))
(prinn (disjoint? '(A) '(B)))

(prinn (setcomplement '(A B C) '(C D)))
;;
(prinn (union A (setcomplement A U)))
;;
(prinn (cartesian-product A B))
(prinn (cartesian-product '(1 2 3 4) '(a b c d)))
;;
;;
;; Sorites Paradox and Fuzzy Sets
;;
(defun clean-fuzzy (a)
  (let ((la (cardinality a))
        (out '()))
    (loop for i from 0 below la
          do (setf out (cons (first (nth i a)) out)))
    (reverse out)))
;;
;;
(defun union-fuzzy (a b)
  (let* ((temp (union (clean-fuzzy a) (clean-fuzzy b)))
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
(defun intersection-fuzzy (a b)
  (let* ((temp (intersection (clean-fuzzy a) (clean-fuzzy b)))
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
(defun complement-fuzzy (a)
  (let ((lA (cardinality a))
        (out '()))
    (loop for i from 0 below lA
          do (let* ((el (nth i a)))
                   (setf out (cons (list (first el) (- 1.0 (car (last el)))) 
                                   out))))
    (reverse out)))
;;
(defun cartesian-product-fuzzy (a b)
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
(defun belongs?-fuzzy (set x)
  (if (and (>= x (nth 1 set)) (<= x (nth 4 set)))
      t nil))
;;
(defun set-member?-fuzzy (set x)
  (let ((name (nth 0 set))
        (x1 (nth 1 set))
        (x2 (nth 2 set))
        (x3 (nth 3 set))
        (x4 (nth 4 set)))
  (cond 
    ((or (<= x x1) (>= x x4))
     (list name 0.0))
    ((and (> x x1) (< x x2))
     (list name (/ (* (- x x1) 1.0) (- x2 x1))))
    ((and (>= x x2) (<= x x3))
     (list name 1.0))
    ((and (> x x3) (< x x4))
     (list name (/ (* (- x4 x) 1.0) (- x4 x3)))))))
;;
(defun alpha-cut-fuzzy (set alpha)
  (let ((name (nth 0 set))
        (x1 (nth 1 set))
        (x2 (nth 2 set))
        (x3 (nth 3 set))
        (x4 (nth 4 set))
        (ex_l 0)
        (ex_r 0))
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
        (setf m1 single-float-positive-infinity)
        (setf m1 (aux-calculate-m-fuzzy x1a x2a alpha1 alpha2)))
    (if (< (abs (- x1b x2b)) 0.000001)
        (setf m2 single-float-positive-infinity)
        (setf m2 (aux-calculate-m-fuzzy x1b x2b alpha1 alpha2)))
    (setf base1 (- x1a (/ alpha1 m1)))
    (if (< m2 0.0)
        (setf m2 (* m2 -1.0)))
    (setf base4 (+ (/ alpha1 m2) x1b))
    (setf base2 (/ (+ 1 (* m1 base1)) m1))
    (setf base3 (/ (- (* m2 base4) 1.0) m2))
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
(defun discretise-fuzzy (set steps)
  (let* ((name (nth 0 set))
        (x1 (nth 1 set))
        (x2 (nth 2 set))
        (x3 (nth 3 set))
        (x4 (nth 4 set))
        (out (list name))
        (trap t)
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
          (setf trap nil))
        (progn 
          (setf resolution (/ (- x3 x2) steps))
          (setf x x2)
          (loop for i from 0 below steps
                do (progn
                     (setf out (cons (list x 1.0) out))
                     (setf x (+ x resolution))))))
    (setf resolution (/ (- x4 x3) steps))
    (if trap
        (setf x x3)
        (setf x (+ x3 resolution)))
    (loop for i from 0 below steps
          do (progn
               (setf out (cons (list x (car (last (set-member?-fuzzy set x)))) 
                               out))
               (setf x (+ x resolution))))
    (when trap
      (setf out (cons (list x 0.0) out)))
    (reverse out)))
;;
(defun dset-member?-fuzzy (dfset x)
  (let ((result (list (first dfset) 0.0))
        (n (length dfset)))
    (loop for i from 1 below (- n 1)
          do (progn 
               (let ((paira (nth i dfset))
                     (pairb (nth (+ 1 i) dfset)))
                 (if (and (<= (first paira) x) 
                          (>= (first pairb) x))
                     (setf result (list (first dfset) 
                                        (interpolation paira pairb (- x (first paira)))))))))
    result))
;;
(defun interpolation (pa pb p)
  (let ((a (first pa))
        (b (first pb))
        (y1 (car (last pa)))
        (y2 (car (last pb))))
    (+ y1 (/ (* p (- y2 y1)) (- b a)))))
;;
(defun discretise-fn-fuzzy (name fn steps a b)
  (let ((out (list name))
        (resolution (/ (- b a) steps)))
    (loop with x = a while (<= x (+ b 0.00001))
          do (progn
               (setf out (cons (list x (funcall fn x)) out))
               (setf x (+ x resolution))))
    (reverse out)))
;;
(defun set-complement-membership?-fuzzy (set x)
  (list (first (set-member?-fuzzy set x))
        (- 1.0 (car (last (set-member?-fuzzy set x))))))
;;
(defun set-union-membership?-fuzzy (name set1 set2 x)
  (let ((mu1 (car (last (set-member?-fuzzy set1 x))))
        (mu2 (car (last (set-member?-fuzzy set2 x)))))
    (list name (max mu1 mu2))))
;;
(defun set-intersect-membership?-fuzzy (name set1 set2 x)
  (let ((mu1 (car (last (set-member?-fuzzy set1 x))))
        (mu2 (car (last (set-member?-fuzzy set2 x)))))
    (list name (min mu1 mu2))))
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
(defun add-fuzzy (name a b)
  (let ((cut1a (alpha-cut-fuzzy a 0.25))
        (cut1b (alpha-cut-fuzzy b 0.25))
        (cut2a (alpha-cut-fuzzy a 0.75))
        (cut2b (alpha-cut-fuzzy b 0.75)))
    (pop cut1a)
    (pop cut1b)
    (pop cut2a)
    (pop cut2b)
    (let ((sum1 (intv-add-fuzzy (nth 0 cut1a) (nth 1 cut1a) 
                                (nth 0 cut1b) (nth 1 cut1b)))
          (sum2 (intv-add-fuzzy (nth 0 cut2a) (nth 1 cut2a)
                                (nth 0 cut2b) (nth 1 cut2b))))
      (push '0.25 sum1)
      (push '0.75 sum2)
      (def-set-fuzzy name sum1 sum2))))
;;
(defun sub-fuzzy (name a b)
  (let ((cut1a (alpha-cut-fuzzy a 0.25))
        (cut1b (alpha-cut-fuzzy b 0.25))
        (cut2a (alpha-cut-fuzzy a 0.75))
        (cut2b (alpha-cut-fuzzy b 0.75)))
    (pop cut1a)
    (pop cut1b)
    (pop cut2a)
    (pop cut2b)
    (let ((sum1 (intv-sub-fuzzy (nth 0 cut1a) (nth 1 cut1a) 
                                (nth 0 cut1b) (nth 1 cut1b)))
          (sum2 (intv-sub-fuzzy (nth 0 cut2a) (nth 1 cut2a)
                                (nth 0 cut2b) (nth 1 cut2b))))
      (push '0.25 sum1)
      (push '0.75 sum2)
      (def-set-fuzzy name sum1 sum2))))
;;
;;
(defun mult-fuzzy (name a b n)
  (let ((head '())
        (tail '())
        (interval (/ 1.0 n))
        (alpha 0.0))
    (loop for i from 0 upto n
          do (let* ((cutA (rest (alpha-cut-fuzzy a alpha)))
                    (cutB (rest (alpha-cut-fuzzy b alpha)))
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
          do (let* ((cutA (rest (alpha-cut-fuzzy a alpha)))
                    (cutB (rest (alpha-cut-fuzzy b alpha)))
                    (div (intv-div-fuzzy 
                          (first cutA) (car (last cutA)) (first cutB) (car (last cutB)))))
               (setf head (cons (append (list (first div)) 
                                        (list alpha)) 
                                head))
               (setf tail (cons (append (list (car (last div))) 
                                        (list alpha))
                                tail))
               (setf alpha (+ interval alpha))))
    (append (list name) (reverse head) (rest tail))))
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
;;
(defvar fA '((1 0.7) (2 0.1) (3 0.3) (4 0.9) (5 0.2)))
(defvar fB '((1 0.1) (2 0.8) (3 0.9) (4 0.2) (5 1.0)))
;;
(prinn (clean-fuzzy fA))
(prinn (clean-fuzzy fB))
;;
(prinn (union-fuzzy fA fB))
(prinn (intersection-fuzzy fA fB))
(prinn (intersection-fuzzy fA fA))
;;
(prinn (complement-fuzzy fA)) 
;;
;; Fuzzy sets don't conform to the Law of Non-Contradiction.
(prinn (intersection-fuzzy fA (complement-fuzzy fA)))
;;
(prinn (cartesian-product-fuzzy fA fB))
;;
(defvar cold '(cold 0.0 0.0 10.0 20.0))
(defvar comf '(comf 10.0 20.0 30.0 40.0))
(defvar warm '(warm 30.0 40.0 100.0 100.0))
;;
(prinn (belongs?-fuzzy cold '15))
(prinn (belongs?-fuzzy cold '25))
;;
(prinn (set-member?-fuzzy comf 25))
(prinn (set-member?-fuzzy cold 25))
(prinn (set-member?-fuzzy comf 35))
(prinn (set-member?-fuzzy warm 35))
;;
;;
(prinn (alpha-cut-fuzzy '(B1 7 10 12 15) 0.7))
(prinn (alpha-cut-fuzzy '(B1 7 10 12 15) 0.0))
(prinn (alpha-cut-fuzzy '(B1 7 10 12 15) 1.0))
;;
(prinn (def-set-fuzzy 'young '(15.0 35.0 0.0) '(25.0 25.0 1.0)))
(prinn (def-set-fuzzy 'mature '(35.0 75.0 0.0) '(45.0 55.0 1.0)))
;;
(defvar dA (discretise-fuzzy '(B1 7 10 12 15) 4))
(prinn dA)
(prinn (discretise-fuzzy '(B1 7 10 12 15) 8))
;;
(prinn (dset-member?-fuzzy dA 8.21))
;;
;; y = (1 + cos(2pi(x-2)))/2
(setf f1 (lambda (x) (/ (+ 1.0 (cos (* 2.0 pi (- x 2.0)))) 2.0)))
(prinn (discretise-fn-fuzzy 'Bell f1 20 1.5 2.5))
;;
(prinn (set-complement-membership-fuzzy '(B1 7 10 12 15) 9))
;;
(defvar AA '(Triangle 1 5 5 10))
(defvar BB '(Trapezium 5 10 15 20))
;;
(prinn (set-union-membership?-fuzzy 'AuB AA BB 7.5))
(prinn (set-intersect-membership?-fuzzy 'AintB AA BB 8))
;;
;;
(prinn (intv-add-fuzzy 2 4 1 3))
(prinn (intv-sub-fuzzy 2 4 1 3))
(prinn (intv-mult-fuzzy 2 4 1 3))
(prinn (intv-div-fuzzy 2 4 1 3))
;;
(prinn (def-set-fuzzy 'A+B '(6.6625 7.3375 0.25) '(6.8875 7.1125 0.75)))
;;
(defvar AAA '(around-2 1.75 2 2 2.25))
(defvar BBB '(around-5 4.8 5 5 5.2))
;;
(prinn (add-fuzzy 'A+B AA BB))
(prinn (sub-fuzzy 'A-B AA BB))
;;
(prinn (mult-fuzzy 'AxB AA BB 5))
(prinn (div-fuzzy 'A/B AA BB 5))
(prinn (div-fuzzy 'B/A BB AA 5))
;;
(prinn (dset-member?-fuzzy (div-fuzzy 'B/A BB AA 5) 2.4))
;;
(prinn (fuzzy-factor '(A1 -2 3 3 8) 0.25))
(prinn (fuzzy-factor '(A1 -2 3 3 8) 3))