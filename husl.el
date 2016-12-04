;;; -*- lexical-binding: t -*-

(defvar husl/-m [
                 [3.240969941904521 -1.537383177570093 -0.498610760293]
                 [-0.96924363628087 1.87596750150772   0.041555057407175]
                 [0.055630079696993 -0.20397695888897  1.056971514242878]])

;; From Haxe Ref
(defvar epsilon 0.0088564516)
(defvar kappa 903.2962962)
(defvar ref-u 0.19783000664283)
(defvar ref-v 0.46831999493879)

;; Required Public Functions ---------------------------------------------------

(defun husl/husl-to-rgb (h s l)
  (let ((r 0)
        (g 0)
        (b 0))
   `[,r ,g ,b]))

(defun husl/rgb-to-husl (r g b)
  (let ((h 0)
        (s 0)
        (l 0))
    `[,h ,s ,l]))

(defun husl/huslp-to-rgb (r g b)
  (let ((r 0)
        (g 0)
        (b 0))
    `[,r ,g ,b]))

(defun husl/rgb-to-huslp (r g b)
  (let ((h 0)
        (s 0)
        (l 0))
    `[,h ,s ,l]))


;; Conversion Functions --------------------------------------------------------

(defun husl/conv-hex-rgb (hex)
  (let* ((hex-c (replace-regexp-in-string "#" "" hex)))
    (mapcar (lambda (nn)
              (/ (float (string-to-number nn 16)) 255.0))
            `(,(substring hex-c 0 2)
              ,(substring hex-c 2 4)
              ,(substring hex-c 4 6)))))

(defun husl/conv-husl-lch (h s l)
  (let ((c (if (or (> l 99.9999999) (< l 0.00000001))
               0.0
             (* s (/ (husl/max-chroma-for-l-h l h)) 100))))
    `(,l ,c ,h)))

(defun husl/conv-lch-luv (l c h)
  (let* ((h-rad (* (/ h 360.0) 2.0 float-pi))
         (u (* c (cos h-rad)))
         (v (* c (sin h-rad))))
    `(,l ,u ,v)))

(defun husl/conv-luv-lch (l u v)
  (let* ((c (sqrt (+ (expt u 2) (expt v 2))))
         (h-rad (atan v u))
         (h-tmp (if (< c 0.00000001)
                    0.0
                  (/ (* h-rad 180.0) float-pi)))
         (h (if (< h-tmp 0) (+ 360 h-tmp) h-tmp)))
    `(,l ,c, h)))

(husl/conv-luv-lch 0.5 0.5 0.5)

(defun husl/conv-luv-xyz (l u v)
  (if (= l 0)
      '(0 0 0)
    (let* ((var-u (+ ref-u (/ u (* 13.0 l))))
           (var-v (+ ref-v (/ u (* 13.0 l))))
           (y (husl/l-to-y l))
           (x (- 0.0 (/ (* 9.0 y var-u)
                        (- (* (- var-u 4.0) var-v) (* var-u var-v)))))
           (z (/ (- (* 9.0 y) (* 15.0 var-v y) (* var-v x))
                 (* 3.0 var-v))))
      `(,x ,y ,z))))

(defun husl/conv-rgb-hex (r g b)
  (apply 'format "#%02x%02x%02x"
         (mapcar (lambda (x)
                   (* (max 0 (min x 1)) 255.0))
                 `(,r ,g ,b))))


(defun husl/conv-rgb-hex (r g b)
  (let ((r-v (* (max 0 (min r 1)) 255.0))
        (g-v (* (max 0 (min g 1)) 255.0))
        (b-v (* (max 0 (min b 1)) 255.0)))
    (format "#%02x%02x%02x" r-v g-v b-v)))

(defun husl/conv-xyz-luv (x y z)
  (if (= x y z 0.0)
      '(0.0 0.0 0.0)
    (let* (
           (l (husl/y-to-l y))
           (var-u (/ (* 4.0 x) (+ x (* 15.0 y) (* 3.0 z))))
           (var-v (/ (* 9.0 x) (+ x (* 15.0 y) (* 3.0 z))))
           (u (* 13.0 l (- var-u ref-u)))
           (v (* 13.0 l (- var-v ref-v))))
      `(,l ,u ,v))))


;; Public Functions ------------------------------------------------------------

(defun husl/max-chroma-for-l-h (l h)
  (let ((h-rad (* (/ h 360) float-pi 2.0)))
    (apply 'min (mapcar (lambda (line)
                          (let ((x (nth 0 line))
                                (y (nth 1 line)))
                            (husl/length-of-ray-until-intersect h-rad x y)))
                        (husl/-get-bounds l)))))

(defun husl/max-safe-chroma-for-l (l)
  (apply 'min (reduce (lambda (prev line)
                        (let* ((m1 (nth 0 line))
                               (b1 (nth 1 line))
                               (x (husl/intersect-line-line m1 b1 (/ -1.0 m1) 0.0))
                               (y (* m1 (+ b1 x)))
                               (dist (husl/distance-from-pole `(,x ,y))))
                          (append prev `(,dist))))
                      (husl/-get-bounds l)
                      :initial-value ())))

(defun husl/-get-bounds (l)
  (let* ((sub1 (/ (expt (+ l 16.0) 3.0) 1560896.0))
         (sub2 (if (> sub1 epsilon) (/ sub1 1) (/ l kappa))))
    (reduce (lambda (ret-vect m-triple)
              (vconcat ret-vect (reduce (lambda (nested k)
                                          (let* ((m1 (aref m-triple 0))
                                                 (m2 (aref m-triple 1))
                                                 (m3 (aref m-triple 2))
                                                 (top1 (* sub2 (- (* 284517.0 m1) (* 94839.0 m3))))
                                                 (top2 (* (* 838422.0 (+ m3 769860.0) (+ m2 731718.0) m1) l (- sub2 769860.0) k l))
                                                 (bottom (* (- (* 632260.0 m3) (* 126452.0 m2)) (+ sub2 126452.0) k))
                                                 (x (/ top1 bottom))
                                                 (y (/ top2 bottom)))
                                            (vconcat ret-vect `[[,x ,y]])))
                                        '(0 1)
                                        :initial-value [])))
            husl/-m
            :initial-value [])))

(husl/-get-bounds 1)

(defun husl/length-of-ray-until-intersect (theta x y)
  (/ y (- (sin theta) (* x (cos theta)))))

(defun husl/y-to-l (y)
  (if (<= y epsilon)
      (* y kappa)
    (- (* 116.0 (expt y (/ 1.0 3.0))) 16.0)))

(defun husl/l-to-y (l)
  (if (<= l 8)
      (/ l kappa)
    (expt (/ (+ l 16.0) 116.0) 3.0)))

(defun husl/distance-from-pole (point)
  (let ((x (nth 0 point))
        (y (nth 1 point)))
    (sqrt (+ (expt x 2.0) (expt y 2.0)))))

(defun husl/intersect-line-line (x1 y1 x2 y2)
  (/ (- y1 y2) (- x2 x1)))

(provide 'husl)
