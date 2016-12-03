(require 'json)

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
    (let ((hex   (car element))
          (husl  (cdr (assoc 'husl (cdr element))))
          (huslp (cdr (assoc 'huslp (cdr element))))
          (lch   (cdr (assoc 'lch (cdr element))))
          (luv   (cdr (assoc 'luv (cdr element))))
          (rgb   (cdr (assoc 'rgb (cdr element))))
          (xyz   (cdr (assoc 'xyz (cdr element)))))
      (eval `(ert-deftest ,hex ()
               (should (eq-triple ,husl ,husl))
               (should (eq-triple ,huslp ,huslp))
               (should (eq-triple ,lch ,lch))
               (should (eq-triple ,luv ,luv))
               (should (eq-triple ,rgb ,rgb))
               (should (eq-triple ,xyz ,xyz)))))))
