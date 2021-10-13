(load "src/random.lisp")

(defun get-x (l) (first l))
(defun get-y (l) (second l))

(defun make-point-between (&key width height)
  (list
    (random-number-to width)
    (random-number-to height)))

;; (defun calc-next-point-area
;;   (&key
;;     last-point
;;     point
;;     artwork-width
;;     artwork-height
;;   ))