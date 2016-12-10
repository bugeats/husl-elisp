;;; -*- lexical-binding: t -*-

(require 'cl)
(cl-float-limits)

;; Constants -------------------------------------------------------------------

(defvar husl/-m [
                 [3.240969941904521 -1.537383177570093 -0.498610760293]
                 [-0.96924363628087 1.87596750150772   0.041555057407175]
                 [0.055630079696993 -0.20397695888897  1.056971514242878]])

(defvar husl/-minv [
                    [0.41239079926595  0.35758433938387 0.18048078840183]
                    [0.21263900587151  0.71516867876775 0.072192315360733]
                    [0.019330818715591 0.11919477979462 0.95053215224966]])

(defvar husl/-ref-y 1.0)
(defvar husl/-ref-u 0.19783000664283)
(defvar husl/-ref-v 0.46831999493879)

(defvar husl/-kappa 903.2962962)
(defvar husl/-epsilon 0.0088564516)

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

;; -----------------------------------------------------------------------------

(defun husl/-get-bounds (l)
  "For a given lightness, return a list of 6 lines in slope-intercept form that \
represent the bounds in CIELUV, stepping over which will push a value out of the RGB gamut"
  (let* ((sub1 (/ (expt (+ l 16.0) 3.0) 1560896.0))
         (sub2 (if (> sub1 husl/-epsilon) (/ sub1 1) (/ l husl/-kappa))))
    (reduce (lambda (ret-vect m-triple)
              (vconcat ret-vect
                       (reduce (lambda (nested k)
                                 (let* ((m1 (aref m-triple 0))
                                        (m2 (aref m-triple 1))
                                        (m3 (aref m-triple 2))
                                        (top1 (* sub2 (- (* 284517.0 m1) (* 94839.0 m3))))
                                        (top2 (- (* (+ (* 838422.0 m3) (* 769860.0 m2) (* 731718.0 m1)) l sub2)
                                                 (* 769860 k l)))
                                        (bottom (+ (* (- (* 632260.0 m3) (* 126452.0 m2)) sub2)
                                                   (* 126452.0 k)))
                                        (x (/ top1 bottom))
                                        (y (/ top2 bottom)))
                                   (vconcat nested `[[,x ,y]])))
                               [0 1]
                               :initial-value [])))
            husl/-m
            :initial-value [])))

(defun husl/-max-safe-chroma-for-l (l)
  "For given lightness, returns the maximum chroma. Keeping the chroma value \
below this number will ensure that for any hue, the color is within the RGB gamut."
  (apply 'min (reduce (lambda (prev line)
                        (let* ((m1 (aref line 0))
                               (b1 (aref line 1))
                               (x (husl/intersect-line-line m1 b1 (/ -1.0 m1) 0.0))
                               (y (* m1 (+ b1 x)))
                               (dist (husl/distance-from-pole `(,x ,y))))
                          (append prev `(,dist))))
                      (husl/-get-bounds l)
                      :initial-value ())))

(defun husl/-length-of-ray-until-intersect (theta x y)
  (let ((length (/ y (- (sin theta) (* x (cos theta))))))
    (cond ((< length 0)
           nil)
          ((isnan length)
           nil)
          (t
           length))))

(defun husl/-max-chroma-for-l-h (l h)
  (let ((h-rad (* (/ h 360) float-pi 2.0)))
    (apply 'min (reduce (lambda (prev line)
                          (let* ((x (aref line 0))
                                 (y (aref line 1))
                                 (length (husl/-length-of-ray-until-intersect h-rad x y)))
                            (if (eq length nil)
                                prev
                                (append prev `(,length)))))
                        (husl/-get-bounds l)
                        :initial-value `(,cl-most-positive-float)))))

(defun husl/-y-to-l (y)
  "In these formulas, Yn refers to the reference white point. We are using illuminant D65, \
so Yn (see refY in Maxima file) equals 1. The formula is simplified accordingly. \
http://en.wikipedia.org/wiki/CIELUV"
  (if (<= y husl/-epsilon)
      (* husl/-kappa (/ y husl/-ref-y))
    (- (* 116 (expt (/ y husl/-ref-y) (/ 1.0 3.0))) 16)))

(defun husl/-l-to-y (l)
  (if (<= l 8)
      (/ (* husl/-ref-y l) husl/-kappa)
    (* husl/-ref-y (expt (/ (+ l 16.0) 116.0) 3.0))))

;; Conversion Functions --------------------------------------------------------

(defun husl/-hex-to-rgb (hex)
  (let* ((hex-c (replace-regexp-in-string "#" "" hex)))
    (cl-map 'vector (lambda (nn)
                      (/ (float (string-to-number nn 16)) 255.0))
                    `[,(substring hex-c 0 2)
                      ,(substring hex-c 2 4)
                      ,(substring hex-c 4 6)])))

(defun husl/-husl-to-lch (h s l)
  (let ((c (if (or (> l 99.9999999) (< l 0.00000001))
               0.0
             (* s (/ (husl/-max-chroma-for-l-h l h)) 100.0))))
    (vector l c h)))

(defun husl/-lch-to-husl (l c h)
  (cond ((> l 99.9999999)
         (vector h 0.0 100.0))
        ((< l 0.00000001)
         (vector h 0.0 0.0))
        (t
          (let* ((max (husl/-max-chroma-for-l-h l h))
                 (s (* (/ c max) 100.0)))
            (vector h s l)))))

(defun husl/-lch-to-luv (l c h)
  (let* ((h-rad (* (/ h 360.0) 2.0 float-pi))
         (u (* c (cos h-rad)))
         (v (* c (sin h-rad))))
    (vector l u v)))

(defun husl/-luv-to-lch (l u v)
  (let* ((c (sqrt (+ (expt u 2) (expt v 2))))
         (h-rad (atan v u))
         (h-tmp (if (< c 0.00000001)
                    0.0
                  (/ (* h-rad 180.0) float-pi)))
         (h (if (< h-tmp 0) (+ 360 h-tmp) h-tmp)))
    (vector l c h)))

(defun husl/-luv-to-xyz (l u v)
  (if (= l 0)
      '(0 0 0)
    (let* ((var-u (+ husl/-ref-u (/ u (* 13.0 l))))
           (var-v (+ husl/-ref-v (/ u (* 13.0 l))))
           (y (husl/-l-to-y l))
           (x (- 0.0 (/ (* 9.0 y var-u)
                        (- (* (- var-u 4.0) var-v) (* var-u var-v)))))
           (z (/ (- (* 9.0 y) (* 15.0 var-v y) (* var-v x))
                 (* 3.0 var-v))))
      (vector x y z))))

(defun husl/-rgb-to-hex (r g b)
  (apply 'format "#%02x%02x%02x"
         (mapcar (lambda (x)
                   (* (max 0 (min x 1)) 255.0))
                 (vector r g b))))

(defun husl/-xyz-to-luv (x y z)
  (let* ((divider (+ x (* 15 y) (* 3 z)))
         (var-u (if (= divider 0)
                    0.0e+NaN
                  (/ (* 4 x) divider)))
         (var-v (if (= divider 0)
                    0.0e+NaN
                  (/ (* 9 y) divider)))
         (l (husl/-y-to-l y)))
    (if (= l 0)
        (vector 0 0 0)
      (vector l
              (* 13 l (- var-u husl/-ref-u))
              (* 13 l (- var-v husl/-ref-v))))))

;; -----------------------------------------------------------------------------

(defun husl/-from-linear (c)
  (if (<= c 0.0031308)
      (* 12.92 c)
    (- (* 1.055 (expt c (/ 1 2.4))) 0.055)))

(defun husl/-dot-product (v1 v2)
  (let ((res 0))
    (dotimes (i (length v1))
      (setq res (+ res (* (elt v1 i) (elt v2 i)))))
    res))

(defun husl/-xyz-to-rgb (x y z)
  "XYZ coordinates are ranging in [0:1] and RGB coordinates in [0:1] range.\n
Return an array containing the resulting color's red, green and blue."
  (let ((tuple `[,x ,y ,z]))
    (vector (husl/-from-linear (husl/-dot-product (elt husl/-m 0) tuple))
            (husl/-from-linear (husl/-dot-product (elt husl/-m 1) tuple))
            (husl/-from-linear (husl/-dot-product (elt husl/-m 2) tuple)))))

(defun husl/-rgb-to-xyz (r g b)
  (let ((rgbl `[,(husl/-from-linear r)
                ,(husl/-from-linear g)
                ,(husl/-from-linear b)]))
    (vector (husl/-dot-product (elt husl/-minv 0) rgbl)
            (husl/-dot-product (elt husl/-minv 1) rgbl)
            (husl/-dot-product (elt husl/-minv 2) rgbl))))

;; -----------------------------------------------------------------------------

(defun husl/distance-from-pole (point)
  (let ((x (nth 0 point))
        (y (nth 1 point)))
    (sqrt (+ (expt x 2.0) (expt y 2.0)))))

(defun husl/intersect-line-line (x1 y1 x2 y2)
  (/ (- y1 y2) (- x2 x1)))

(provide 'husl)
