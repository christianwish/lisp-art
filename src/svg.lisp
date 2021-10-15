(load "src/random.lisp")

(defun d (points &optional (acc ""))
  (if (= (length points) 0)
      ;;acc
      (concatenate 'string acc " Z")
      (let* ((prefix (if (= (length acc) 0) "M" "L"))
             (point (first points))
             (x (write-to-string (first point)))
             (y (write-to-string (second point)))
             (str (concatenate 'string acc prefix x " " y " ")))
            (d (rest points) str)
        )))

(defun to-path (points)
  (concatenate
    'string
    "<path fill=\"rgba(0, 0, 0, 0.2)\" stroke=\"#fff\" d=\""
    (d points)
    "\" stroke-width=\""
    (write-to-string (random-number-between 4 70))
    "\" />"))