; sbcl --script compile.lisp 

(require "app" "./app/app.lisp")

(defun run-main () (app:main (rest *posix-argv*)))

(sb-ext:save-lisp-and-die "./dist/lisp-art"
:executable t
:toplevel 'run-main)
