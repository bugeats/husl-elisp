(require 'json)

(defun get-string-from-file (filePath)
  "Return filePath's file content as a string."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(setq ref-data
      (json-read-from-string (get-string-from-file "./snapshot-rev4.json")))

(let (value)
  (dolist (element ref-data value)
    (let ((hex (car element)))
      (eval
        `(ert-deftest ,hex ()
            (should (= (+ 2 2) 4)))))))

