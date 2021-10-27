(defpackage :coordinates
  (:use :common-lisp)
  (:export
    #:point
    #:make-point
    #:show
    ))

(load "packages/random")

(defparameter *app-param-left-wight* 17)
(defparameter *app-param-straight-wight* 2)
(defparameter *app-param-right-wight* 7)
(defparameter *app-param-max-hypotenuse-length* 0.3)
(defparameter *app-param-min-hypotenuse-length* 0.03)
(defparameter *app-param-angle-sector* 20.0)


(defclass point ()
  ((x :initarg :x :accessor x :type float
    :documentation "X is the distance from left")
   (y :initarg :y :accessor y :type float
    :documentation "Y is the distance from top")
   (bezir1 :initarg  nil :accessor bezir1 :type point
    :documentation "X is the distance from left")
   (bezir2 :initarg  nil :accessor bezir2 :type point
    :documentation "X is the distance from left")))


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

(defun get-random-direction ()
  (random-item-via-weight
    ("left"     17)  ;; TODO: app-param
    ("straight" 2)   ;; TODO: app-param
    ("right"    7))) ;; TODO: app-param

(defun point-inside-p (&key point width height)
  (let ((x (x point))
        (y (y point)))
    (and (>= x 0) (<= x width) (>= y 0) (<= y height))))

(defun alpha-from-absolute (angle)
  (cond
    ((= angle 0) 90)
    ((= angle 90) 0)
    ((= angle 180) 90)
    ((= angle 270) 0)
    ((and (> angle 0) (< angle 90)) (- 90 angle))
    ((and (> angle 90) (< angle 180)) (- 90 (- 180 angle)))
    ((and (> angle 180) (< angle 270)) (- 90 (- 270 angle)))
    (t (- 90 (- 360 angle)))))

;; TODO: improve angle calculation
(defun get-next-point (&key points width height (i 0) )
  (labels ((get-random-angle (fn angle)
              (let ((a1 (funcall fn angle (* *app-param-angle-sector* 1.5)))
                    (a2 (funcall fn angle (* *app-param-angle-sector* 0.5))))
                   (random-number-from-to a1 a2)))
            (get-fake-p1 (p1 p2)
              (let* ((x1 (x p1))
                     (y1 (y p1))
                     (x2 (x p2))
                     (y2 (y p2))
                     (new-x (if (<= x1 x2) (- x1 (- x2 x1)) (+ x2 (- x2 x1))))
                     (new-y (if (<= y1 y2) (- y1 (- y2 y1)) (+ y2 (- y2 y1)))))
                (x&y new-x new-y)))
            (get-random-length ()
              (let* ((max-absolute (min width height))
                     (max-relative
                        (random-number-from-to
                          (round (* max-absolute *app-param-min-hypotenuse-length*))
                          (round (* max-absolute *app-param-max-hypotenuse-length*))
                          :never-zero t)))
                    max-relative)))
    (let* ((last-two-points (last points 2))
           (p1-origin (first last-two-points))
           (p2 (second last-two-points))
           (p1-fake (get-fake-p1 p1-origin p2))
           (p1 (if (<= i 100) p1-origin p1-fake))
           (triangle (make-triangle-info p1 p2))
           (min-distance
              (minimum-distance-to-frame :point p2 :width width :height height))
           (direction (get-random-direction))
           (alpha-abs (alpha-absolute triangle))
           (new-absolute-alpha
              (cond ((equal "left" direction) (get-random-angle #'+ alpha-abs))
                    ((equal "right" direction) (get-random-angle #'- alpha-abs))
                    (t alpha-abs)))
           (new-alpha (alpha-from-absolute new-absolute-alpha))
           (new-hypotenuse-length (get-random-length))
           (a (* new-hypotenuse-length (sin new-alpha)))
           (b (* new-hypotenuse-length (cos new-alpha)))
           (new-x (if (<= new-absolute-alpha 180) (+ width b) (- width b)))
           (new-y (if (and (>= new-absolute-alpha 90) (<= new-absolute-alpha 270)) (+ height a) (- height a)))
           (new-point (x&y new-x new-y))
           (new-point-valid (point-inside-p :point new-point :width width :height height)))
           (cond
              (new-point-valid new-point)
              ((> i 200) new-point)
              (t (get-next-point :points points :width width :height height :i (+ i 1)))))))


(defun create-path-points (&key width height)
  (let* ((start-points
            (list
              (x&y (random-number-to width) (random-number-to height))
              (x&y (random-number-to width) (random-number-to height))))
         (point-length (random-number-from-to 3 4)))

    (defun run-it (points)
      (if (= (length points) point-length)
          points
          (run-it (append points (list (get-next-point
                                        :points points
                                        :width width
                                        :height height))))))
    (run-it start-points)))

;;;; show -------------------------------------------------
(defgeneric show (obj))

(defmethod show ((obj point))
  (let ((x (write-to-string (x obj)))
        (y (write-to-string (y obj))))
  (concatenate 'string "(X=" x ",Y=" y ")" )))

(defmethod show ((obj null)) "")
(defmethod show ((obj cons));; TODO
  (concatenate 'string (show (car obj)) (show (cdr obj))))
