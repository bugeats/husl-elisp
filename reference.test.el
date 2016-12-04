(require 'json)
(load-file "./husl.el")

(defun get-string-from-file (filePath)
  "Return filePath's file content as a string."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(setq ref-data
      (json-read-from-string (get-string-from-file "./snapshot-rev4.json")))

(defun eq-triple (a b)
  (and (equal (elt a 0) (elt a 0))
       (equal (elt a 1) (elt b 1))
       (equal (elt a 2) (elt b 2))))

(let (value)
  (dolist (element ref-data value)
    (let* ((ref-key   (car element))
           (ref-hex   (symbol-name ref-key))
           (ref-husl  (cdr (assoc 'husl (cdr element))))
           (ref-huslp (cdr (assoc 'huslp (cdr element))))
           (ref-lch   (cdr (assoc 'lch (cdr element))))
           (ref-luv   (cdr (assoc 'luv (cdr element))))
           (ref-rgb   (cdr (assoc 'rgb (cdr element))))
           (ref-xyz   (cdr (assoc 'xyz (cdr element))))
           (rgb (apply 'vector (husl/conv-hex-rgb ref-hex))))
      (eval `(ert-deftest ,ref-key ()
               (should (eq-triple ,ref-rgb ,rgb))
               (should (eq-triple ,ref-rgb ,ref-rgb))
               (should (eq-triple ,ref-husl ,ref-husl))
               (should (eq-triple ,ref-huslp ,ref-huslp))
               (should (eq-triple ,ref-lch ,ref-lch))
               (should (eq-triple ,ref-luv ,ref-luv))
               (should (eq-triple ,ref-xyz ,ref-xyz)))))))
