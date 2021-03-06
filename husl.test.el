(require 'cl)
(load-file "./husl.el")

(ert-deftest addition-test ()
  (should (= (+ 1 2) 3)))

(ert-deftest required-public-functions-exist ()
  (should (fboundp 'husl/husl-to-rgb))
  (should (fboundp 'husl/huslp-to-rgb))
  (should (fboundp 'husl/rgb-to-husl))
  (should (fboundp 'husl/rgb-to-huslp)))

(ert-deftest test-luv-to-lch ()
  (should (equal (husl/-luv-to-lch
                  66.4684397846863
                  -30.54979105634606
                  -35.56297143225253)
                 [
                  66.4684397846863
                  46.88298913974675
                  229.3363102843981])))

(ert-deftest test-get-bounds ()
  (should (equal (husl/-get-bounds 1)
                 [[-8.021739130434913 -6.370563018241111]
                  [0.008495838735633148 -6.0878608405761545]
                  [1.325964991023324 -3.6495554177681284]
                  [-0.002453300274148112 -6.092671840000601]
                  [-0.12162162162162199 1.1091899201577262]
                  [-0.0007345641271965095 -6.044689744090016]])))

(ert-deftest test-hex-to-rgb ()
  (should (equal (husl/-hex-to-rgb "#DDEEFF")
                 [0.8666666666666667 0.9333333333333333 1.0]))
  (should (equal (husl/-hex-to-rgb "#010101")
                 [0.00392156862745098 0.00392156862745098 0.00392156862745098])))

(ert-deftest test-max-safe-chroma-for-l ()
  (should (equal (husl/-max-safe-chroma-for-l 50) 10.061519430821551)))

(ert-deftest test-y-to-l ()
  (should (equal (husl/-y-to-l 0.5) 76.06926101415557)))

(ert-deftest test-l-to-y ()
  (should (equal (husl/-l-to-y 76.06926101415557) 0.5000000000000001)))

;; TODO this has rounding errors
(ert-deftest test-lch-to-husl ()
  (should (equal (husl/-lch-to-husl 100 50 50)
                 [50 0.0 100.0]))
  (should (equal (husl/-lch-to-husl 0 50 50)
                 [50 0.0 0.0])))
  ;; TODO this has rounding errors
  ;; (should (equal (husl/-lch-to-husl
  ;;                 66.4684397846863
  ;;                 46.88298913974675
  ;;                 229.3363102843981)
  ;;                [
  ;;                 229.3363102843981
  ;;                 66.27255533641578
  ;;                 66.4684397846863])))

;; ;; TODO this has rounding errors
;; (ert-deftest test-lch-to-luv ()
;;   (should (eql (husl/-lch-to-luv
;;                 66.4684397846863
;;                 46.88298913974675
;;                 229.3363102843981)
;;                [
;;                 66.4684397846863
;;                 -30.54979105634606
;;                 -35.56297143225253])))

(ert-deftest test-conv-xyz-luv ()
  (should (equal (husl/-xyz-to-luv 21.24673129498138 7.666298341556465e-15 0)
                 [6.924938897492157e-12 3.422873334643934e-10 -4.216013554252415e-11])))

;; ;; TODO this has rounding errors
;; (ert-deftest test-conv-xyz-luv ()
;;   (should (equal (husl/-xyz-to-rgb 0.3075138663461921
;;                                    0.3593265991736845
;;                                    0.6244394063757823)
;;                  [0.4
;;                   0.6666666666666666
;;                   0.8])))

;; From husl ref
;;
;; #66aacc
;;   rgb
;;     0.4,
;;     0.6666666666666666
;;     0.8
;;   xyz
;;     0.3075138663461921
;;     0.3593265991736845
;;     0.6244394063757823
;;   luv
;;     66.4684397846863
;;     -30.54979105634606
;;     -35.56297143225253
;;   lch
;;     66.4684397846863
;;     46.88298913974675
;;     229.3363102843981
;;   husl
;;     229.3363102843981
;;     66.27255533641578
;;     66.4684397846863
;;   huslp
;;     229.3363102843981
;;     89.5033179536067
;;     66.4684397846863

;; #333333
;;   rgb
;;     0.2
;;     0.2
;;     0.2
;;   xyz
;;     0.03146462160095975
;;     0.033104766570885055
;;     0.03605300262111889
;;   luv
;;     21.24673129498138
;;     7.666298341556465e-15
;;     0
;;   lch
;;     21.24673129498138
;;     7.666298341556465e-15
;;     0
;;   husl
;;     0,
;;     1.3109486654374887e-14
;;     21.24673129498138
;;   huslp
;;     0,
;;     4.5786022494471826e-14
;;     21.24673129498138
