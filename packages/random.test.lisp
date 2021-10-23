(load "packages/random.lisp")
(load "test/tdd.lisp")

(define-test-file random)

(define-test random-number-between-spec
  :func random-number-between
  :tests (
    (:test "gives int when optional parameter `as-float` not given"
     :actual (first (type-of (random-number-between 0 100)))
     :expected 'Integer)

    (:test "gives float when optional parameter `as-float` given"
     :actual (type-of (random-number-between 0 100 t))
     :expected 'SINGLE-FLOAT)

    (:test "gives random number between parameters"
     :is-true (let* ((acc (list))
                     (_whatever (dotimes (i 1000) (push (random-number-between 0 100) acc)))
                     (validLength (length (remove-if #'(lambda (x) (or (< x 0) (> x 100))) acc))))
                (= validLength 1000)))))

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
                     (_whatever (dotimes (i 10000) (push (random-item-via-weight ("a" 1) ("b" 5) ("c" 10)) acc)))
                     (a-length (length (remove-if #'(lambda (x) (not (equal "a" x))) acc)))
                     (b-length (length (remove-if #'(lambda (x) (not (equal "b" x))) acc)))
                     (c-length (length (remove-if #'(lambda (x) (not (equal "c" x))) acc))))
                ;; (print a-length)
                ;; (print b-length)
                ;; (print c-length)
                (< a-length b-length c-length)))))
