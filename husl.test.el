(require 'cl)
(load-file "./husl.el")

(ert-deftest addition-test ()
  (should (= (+ 1 2) 3)))

(ert-deftest test-conv-xyz-luv ()
  (should (equal (husl/conv-xyz-luv 0.5 0.5 0.5)
             '(-1763.9307389858443 -291.14032968045854 -123.00736194996507))))
