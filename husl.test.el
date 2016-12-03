(require 'cl)
(load-file "./husl.el")

(ert-deftest addition-test ()
  (should (= (+ 1 2) 3)))

(ert-deftest test-conv-xyz-luv ()
  (should (eql (husl/conv-xyz-luv
                0.3075138663461921
                0.3593265991736845
                0.6244394063757823)
               '(
                 66.4684397846863
                 -30.54979105634606
                 -35.56297143225253))))


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
