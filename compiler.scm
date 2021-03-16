

(define fxshift 2)
(define fxmask #x03)
(define bool_f #x2F)
(define bool_t #x6F)
(define wordsize 4)
(define niltag  #b00111111)
(define chartag #b00001111)
(define charshift 8)


(define fixnum-bits (- (* wordsize 8) fxshift))
(define fxlower (- (expt 2 (- fixnum-bits 1))))
(define fxupper (sub1 (expt 2 (- fixnum-bits 1))))

(define (fixnum? x)
  (and (integer? x) (exact? x) (<= fxlower x fxupper)))

(define (immediate? x)
  (or (fixnum? x) (boolean? x) (char? x) (null? x)))

(define (immediate-rep x)
  (cond
   [(fixnum? x) (ash x fxshift)]
   [(boolean? x) (if x bool_t bool_f)]
   [(null? x) niltag]
   [(char? x) (logor (ash (char->integer x) charshift) chartag)]))




(define (compile-program x)
  (unless (immediate? x) (error "Not an immediate"))
  (emit "define i32 @scheme_entry(){")
  (emit "   ret i32 ~s" (immediate-rep x))
  (emit "}"))

(load "tests-driver.scm")
(load "tests-1.1-req.scm")
(load "tests-1.2-req.scm")
