(load "packages/random.lisp")
(load "test/tdd.lisp")

(define-test-file random)

(define-test random-number-from-to-spec
  :func random-number-from-to
  :tests (
    (:test "gives int when optional parameter `as-float` not given"
     :actual (first (type-of (random-number-from-to 1 100)))
     :expected 'Integer)

    (:test "gives float when parameter `:as-float` is true"
     :actual (type-of (random-number-from-to 1 100 :as-float t))
     :expected 'SINGLE-FLOAT)

    (:test "gives random number between parameters"
     :is-true (let* ((acc (list))
                     (_whatever (dotimes (i 100) (push (random-number-from-to 1 100) acc)))
                     (validLength (length (remove-if #'(lambda (x) (or (< x 1) (> x 100))) acc))))
                (= validLength 100)))

    (:test "works when first one number is negativ (-33 33)"
     :is-true (let* ((acc (list))
                     (_whatever (dotimes (i 100) (push (random-number-from-to -33 33) acc)))
                     (validLength (length (remove-if #'(lambda (x) (or (< x -33) (> x 33))) acc)))
                     (negativeLength (length (remove-if #'(lambda (x) (>= x 0)) acc))))
                (and (= validLength 100) (> negativeLength 0))))

    (:test "works when both numbers are negativ (-33 -13)"
     :is-true (let* ((acc (list))
                     (_whatever (dotimes (i 100) (push (random-number-from-to -33 -13) acc)))
                     (valid (remove-if #'(lambda (x) (or (< x -33) (> x -13))) acc))
                     (validLength (length valid))
                     (negative (remove-if #'(lambda (x) (> x -13)) acc))
                     (negativeLength (length negative)))
                (and (= validLength 100) (= negativeLength 100))))

    (:test "gives no zero when :never-zero is true"
     :is-true (let* ((acc (list))
                     (_whatever (dotimes (i 1000) (push (random-number-from-to -33 33 :never-zero t) acc)))
                     (validLength (length (remove-if #'(lambda (x) (= 0 x)) acc))))
                (= validLength 1000)))
    (:test "works when first number is bigger than second"
     :is-true (let ((result (random-number-from-to 3 2)))
                   (or (= 2 result) (= 3 result))))

    (:test "works when numbers equal"
     :is-true (= 2 (random-number-from-to 2 2)))))

(define-test random-item-via-weight-spec
  :func random-item-via-weight
  :tests (
    (:test "gives one item of the list"
     :is-true (find
                (random-item-via-weight ("a" 3) ("b" 1) ("c" 10))
                '("a" "b" "c")
                :test #'equal))

    (:test "gives random item via weight"
     :is-true (let* ((acc (list))
                     (_whatever (dotimes (i 1000) (push (random-item-via-weight ("a" 1) ("b" 5) ("c" 10)) acc)))
                     (a-length (length (remove-if #'(lambda (x) (not (equal "a" x))) acc)))
                     (b-length (length (remove-if #'(lambda (x) (not (equal "b" x))) acc)))
                     (c-length (length (remove-if #'(lambda (x) (not (equal "c" x))) acc))))
                (< a-length b-length c-length)))))

(print (random-number-from-to 1 3))