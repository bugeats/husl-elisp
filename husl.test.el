(require 'cl)
(load-file "./husl.el")

(assert (= (husl/conv-xyz-luv 0.5 0.5 0.5)
           '(-1763.9307389858443 -291.14032968045854 -123.00736194996507)))
