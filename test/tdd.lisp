(defpackage :tdd (:use :common-lisp) (:export #:deftest))

(defmacro define-color (function-name color-code)
  `(defun ,function-name (str)
    (format nil (concatenate 'string ,(concatenate 'string "~c[" (write-to-string color-code) "m")  str "~c[0m") #\ESC #\ESC)))

(define-color format-black 30)
(define-color format-cyan 96)
(define-color format-yellow 33)
(define-color format-red 31)
(define-color format-green 32)
(define-color format-grey 37)
(define-color format-underlined 4)
(define-color format-italic 3)
(define-color format-bold 1)
(define-color format-bg-cyan 106)
(define-color format-bg-magenta 105)
(define-color format-magenta 95)

(defun check (&key test actual expected (is-true nil true-check) (skip nil skip-check))
  (if skip-check
    (progn
      (format t (concatenate 'string " ⚠️  [TODO]: " (format-grey test)))
      (terpri t))
    (if true-check
      (let* ((format-fn (if is-true #'format-green #'format-red)))
            (format t (if is-true " 🥝  " " 🔴  "))
            (format t (funcall format-fn test))
            (terpri t))
      (let* ((result (equal actual expected))
            (format-fn (if result #'format-green #'format-red)))
            (format t (if result " 🥝  " " 🔴  "))
            (format t (funcall format-fn test))
            (terpri t)
            (when (not result)
                  (progn
                    (format t (format-grey "   expected:"))
                    (terpri t )
                    (format t (format-red (write-to-string expected)))
                    (terpri t )
                    (format t (format-grey "   actual:"))
                    (terpri t )
                    (format t (format-red (write-to-string  actual)))
                    (terpri t)
                    (terpri t)))))))

(defmacro define-test (test-name &key describe tests func) ;; TODO :func or :macr
  (let ((test-title (string-downcase (string test-name)))
        (function-title (when func (string-downcase (string func))))
        (to-check #'(lambda (all-checks) `(check ,@all-checks))))
      `(progn
        (defun ,test-name ()
          (terpri t)
          (format t (format-bg-cyan (format-black ,test-title)))
          (terpri t)
          (when
           ,function-title
           (format t (format-bg-magenta (format-italic " defun ")))
           (format t " ")
           (format t ,(format-magenta (format-italic function-title))))
          (when ,function-title (terpri t))
          (if ,describe (format t (format-yellow ,describe)) (terpri t))
          (when ,describe (terpri t))
          ,@(mapcar to-check tests))
        (,test-name))))

(defmacro define-test-file (name)
  (let ((test-title (concatenate 'string "RUN TEST SUIT: " (string name) "")))
      `(progn
        (terpri t)
        (format t (format-bold (format-underlined ,test-title)))
        (terpri t))))
