; sbcl --script development.lisp 

(require "app" "./app/app.lisp")

(app:main (rest *posix-argv*))
