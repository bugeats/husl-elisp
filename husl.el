;;; -*- lexical-binding: t -*-

(defvar epsilon 0.0088564516790356308)
(defvar kappa 903.2962962962963)
(defvar ref-u 0.19783000664283681)
(defvar ref-v 0.468319994938791)

(defun husl/conv-xyz-luv (x y z)
  (if (= x y z 0)
      '(0 0 0)
    (let* (
           (l (husl/y-to-l y))
           (var-u (/ (* 4.0 x) (+ x (* 15.0 y) (* 3.0 z))))
           (var-v (/ (* 9.0 x) (+ x (* 15.0 y) (* 3.0 z))))
           (u (* 13.0 l (- var-u ref-u)))
           (v (* 13.0 l (- var-v ref-v))))
      `(,l ,u ,v))))

(defun husl/y-to-l (y)
  (if (<= y epsilon)
    (* y kappa)
    (* 116.0 (- (expt y (/ 1.0 3.0)) 16.0))))

(husl/conv-xyz-luv 0.5 0.5 0.5)

(provide 'husl)
