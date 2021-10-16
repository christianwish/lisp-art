(load "src/random.lisp")
(load "src/io.lisp")
(load "src/point.lisp")

(defun d (points &optional (acc ""))
  (if (= (length points) 0)
      acc
      ;; (concatenate 'string acc " Z")
      (let* ((prefix (if (= (length acc) 0) "M" "L"))
             (point (first points))
             (x (write-to-string (first point)))
             (y (write-to-string (second point)))
             (str (concatenate 'string acc prefix x " " y " ")))
            (d (rest points) str)
        )))

(defun random-rgb (&optional (a 255))
(let ((r (write-to-string (random-number-between 0 255)))
      (g (write-to-string (random-number-between 0 255)))
      (b (write-to-string (random-number-between 0 255))))
  (concatenate 'string "rgba(" r "," g "," b "," (write-to-string a) ")")))

(defun to-path (points)
  (concatenate
    'string
    "<path fill=\"" (random-rgb 130) "\" stroke=\"" (random-rgb) "\" d=\""
    (d points)
    "\" stroke-width=\""
    (write-to-string (random-number-between 7 70))
    "\" />"))

(defun svg/path (&key width height)
  (to-path (remove-zero-points (create-path-points :width width :height height))))

(defun create-svg-file (content &key width height)
  (let* ((timestamp (get-universal-time))
         (h (write-to-string height))
         (w (write-to-string width))
         (file-content (concatenate
                        'string
                        "<svg height=\"" h "\" "
                        "width=\"" w "\" "
                        "viewbox=\"0 0" w " " h "\" >"
                        "<rect height=\"" h "\" "
                        "width=\"" w "\" fill=\"#fff\" />"
                        content
                        "</svg>"
                        ))
        (file-name (concatenate 'string "dist/svg/"(write-to-string timestamp) ".svg")))
  (write-file file-name file-content)
  ))
