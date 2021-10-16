(load "./src/str.lisp")

;; reads user input
(defun ask (question)
  (format *query-io* "~a: " question)
  (force-output *query-io*)
  (read-line *query-io*))

;; reads user input and if nothing was typed, default-value will be used
(defun ask-with-default (question default-value)
  (let* ((answer (ask (concat-all question " [" default-value "]")))
         (result (if (equal answer "") default-value answer)))
    result))

;; better print function
(defgeneric output (object))

(defmethod output ((object string))
  (format t object)
  (terpri t))

(defmethod output (object)
  (print object)
  (terpri t))

(defun write-file (path content)
  (with-open-file (stream
                    path
                    :direction :output    ;; Write to disk
                    :if-exists :supersede ;; Overwrite the file
                    :if-does-not-exist :create)
  (format stream content))
)
