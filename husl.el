;;; -*- lexical-binding: t -*-

(defvar epsilon 0.0088564516790356308)
(defvar kappa 903.2962962962963)
(defvar ref-u 0.19783000664283681)
(defvar ref-v 0.468319994938791)

(defvar husl/m '((3.240969941904521 -1.537383177570093 -0.498610760293)
                 (-0.96924363628087 1.87596750150772   0.041555057407175)
                 (0.055630079696993 -0.20397695888897  1.056971514242878)))

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

;; TODO attempting to make this in a functional style has created a mess.
;; What's idiomatic elisp?
(defun husl/get-bounds (l)
  (let* ((sub1 (/ (expt (+ l 16.0) 3.0) 1560896.0))
         (sub2 (if (> sub1 epsilon) (sub1) (/ l kappa))))
    (reduce (lambda (list m-item)
              (append list (reduce (lambda (nested k)
                                     (let* ((m1 (nth 0 m-item))
                                            (m2 (nth 1 m-item))
                                            (m3 (nth 2 m-item))
                                            (top1 (* sub2 (- (* 284517.0 m1) (* 94839.0 m3))))
                                            (top2 (* (* 838422.0 (+ m3 769860.0) (+ m2 731718.0) m1) l (- sub2 769860.0) k l))
                                            (bottom (* (- (* 632260.0 m3) (* 126452.0 m2)) (+ sub2 126452.0) k))
                                            (x (/ top1 bottom))
                                            (y (/ top2 bottom)))
                                      (append list `((,x ,y)))))
                                  '(0 1)
                                  :initial-value ())))
            husl/m
            :initial-value ())))

(husl/get-bounds 0.5)

(provide 'husl)
