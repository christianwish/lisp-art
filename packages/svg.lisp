(load "packages/random")
(load "packages/coordinates")

(defun d (points &optional (acc ""))
  (if (= (length points) 0)
      acc
      ;;(concatenate 'string acc " Z")
      (let* ((prefix (if (= (length acc) 0) "M" "L"))
             (point (first points))
             (x (format nil "~,5f" (x point)))
             (y (format nil "~,5f" (y point)))
             (str (concatenate 'string acc prefix x " " y " ")))
            (d (rest points) str)
        )))

(defun random-rgb (&optional (a 255))
  (let ((r (write-to-string (random-number-from-to 0 60)))
        (g (write-to-string (random-number-from-to 150 200)))
        (b (write-to-string (random-number-from-to 210 255))))
  (concatenate 'string "rgba(" r "," g "," b "," (write-to-string a) ")")))

(defun to-path (points)
  (concatenate
    'string
    "<path fill=\"" (random-rgb 100) "\" stroke=\"" (random-rgb 0) "\" d=\""
    (d points)
    "\" stroke-width=\""
    (write-to-string (random-number-from-to 2 100))
    "\" "
    "fill-rule=\"nonzero\""
    "/>"))

(defun svg/path (&key width height)
  (to-path (create-path-points :width width :height height)))

(defun create-svg-file (content &key width height)
  (let* ((timestamp (get-universal-time))
         (h (write-to-string height))
         (w (write-to-string width))
         (file-content (concatenate
                        'string
                        "<?xml version=\"1.0\"?>"
                        "<svg xmlns=\"http://www.w3.org/2000/svg\" "
                        "height=\"" h "\" "
                        "width=\"" w "\" "
                        "viewbox=\"0 0" w " " h "\" >"
                        "<rect height=\"" h "\" "
                        "width=\"" w "\" fill=\"#fff\" />"
                        content
                        "</svg>"
                        ))
        (file-name (concatenate 'string "dist/svg/"(write-to-string timestamp) ".svg")))
  (write-file file-name file-content)
  (sb-ext:run-program "/usr/bin/qlmanage" (list "-t" file-name))
  ))
