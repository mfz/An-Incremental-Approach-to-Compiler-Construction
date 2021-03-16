

(define (compile-program x)
  (unless (integer? x) (error ))
  (emit "define i32 @scheme_entry(){")
  (emit "   ret i32 ~s" x)
  (emit "}"))

(load "tests-driver.scm")
(load "tests-1.1-req.scm")
