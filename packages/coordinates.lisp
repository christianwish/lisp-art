(defpackage :coordinates
  (:use :common-lisp)
  (:export
    #:point
    #:make-point
    #:show
    ))

(load "packages/random")

(defparameter *app-param-max-hypotenuse-length* 1/7)
(defparameter *app-param-min-hypotenuse-length* 1/23)
(defparameter *app-param-angle-sector* 33.0)


(defclass point ()
  ((x :initarg :x :accessor x :type float
    :documentation "X is the distance from left")
   (y :initarg :y :accessor y :type float
    :documentation "Y is the distance from top")))


(defclass triangle-info () (
  (a :accessor a :initarg :a :type float)
  (b :accessor b :initarg :b :type float)
  (c :accessor c :initarg :c :type float :documentation "the hypotenuse")
  (alpha :accessor alpha :initarg :alpha :type float)
  (beta :accessor beta :initarg :beta :type float)
  (alpha-absolute :accessor alpha-absolute :initarg :alpha-absolute :type number)))


(defun make-point (&key x y)
  "Creates a point with given X and Y value"
  (make-instance 'point :x (float x) :y (float y)))


(defun x&y (x y)
  "Short for `(make-point :x x :y y)`"
  (make-point :x x :y y))



(defmethod print-object ((obj point) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((x x) (y y)) obj
      (format stream "(X=~a Y=~a)" x y))))


(defun make-triangle-info (point1 point2)
  "Two points define a triangle"
  (let* ((x1 (x point1))
         (x2 (x point2))
         (y1 (y point1))
         (y2 (y point2))
         (a (float (abs (- y1 y2))))
         (b (float (abs (- x1 x2))))
         (a/b (if (or (= a 0) (= b 0)) 0 (/ a b)))
         (hypotenuse (float (sqrt (+ (* a a) (* b b)))))
         (alpha (* (atan a/b) (/ 180 pi)))
         (beta (- 90 alpha))
         (alpha-absolute
          (cond
            ((and (= x1 x2) (= y1 y2)) 0)
            ((and (= x1 x2) (> y1 y2)) 0)
            ((and (= x1 x2) (< y1 y2)) 180)
            ((and (< x1 x2) (= y1 y2)) 90)
            ((and (> x1 x2) (= y1 y2)) 270)
            ((and (< x1 x2) (> y1 y2)) (- 90 alpha))
            ((and (< x1 x2) (< y1 y2)) (+ 90 alpha))
            ((and (> x1 x2) (< y1 y2)) (- 270 alpha))
            (t (+ 270 alpha)))))
        (make-instance 'triangle-info
          :a a
          :b b
          :c (float hypotenuse)
          :alpha (float alpha)
          :alpha-absolute (float alpha-absolute)
          :beta beta)))


(defun minimum-distance-to-frame (&key point width height)
  (let ((x (x point))
        (y (y point)))
    (min x y (- width x) (- height y))))



(defun get-next-point (&key points width height)
  (labels ((get-angle (fn angle)
              (let ((a1 (funcall fn angle (* *app-param-angle-sector* 1.5)))
                    (a2 (funcall fn angle (* *app-param-angle-sector* 0.5))))
                   (random-number-from-to a1 a2)))
           (get-random-direction ()
              (random-item-via-weight
                ("left"     7)   ;; TODO app-param
                ("straight" 2)   ;; TODO app-param
                ("right"    7))) ;; TODO app-param
           )
    (let* ((last-two-points (last points 2))
           (p1 (first last-two-points))
           (p2 (second last-two-points))
           (triangle (make-triangle-info p1 p2))
           (min-distance
              (minimum-distance-to-frame :point p2 :width width :height height))
           (direction (get-random-direction))
           (alpha-abs  (alpha-absolute triangle))
           (new-absolute-angle
              (cond ((equal "left" direction) (get-angle #'+ alpha-abs))
                    ((equal "right" direction) (get-angle #'- alpha-abs))
                    (t alpha-abs))))
          (print new-absolute-angle))))


;;;; show -------------------------------------------------
(defgeneric show (obj))

(defmethod show ((obj point))
  (let ((x (write-to-string (x obj)))
        (y (write-to-string (y obj))))
  (concatenate 'string "(X=" x ",Y=" y ")" )))

(defmethod show ((obj null)) "")
(defmethod show ((obj cons));; TODO
  (concatenate 'string (show (car obj)) (show (cdr obj))))
