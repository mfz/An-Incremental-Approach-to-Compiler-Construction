
(define symcnt 0)

(define (gensym)
  (begin
    (set! symcnt (+ 1 symcnt))
    symcnt))


(define fxshift 2)
(define fxmask #x03)
(define fxtag #x00)
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

(define (compile-immediate x)
  ; return immediate object
  (immediate-rep x))

(define-syntax define-primitive
  (syntax-rules ()
    [(_ (prim-name arg* ...) b b* ...)
     (begin
       (putprop 'prim-name '*is-prim* #t)
       (putprop 'prim-name '*arg-count*
		(length '(arg* ...)))
       (putprop 'prim-name '*emitter*
		(lambda (arg* ...) b b* ...)))]))


(define (primitive? x)
  (and (symbol? x) (getprop x '*is-prim*)))

(define (primitive-emitter x)
  (or (getprop x '*emitter*) (error "Property *emitter* not defined" x)))

(define (primcall? expr)
  (and (pair? expr) (primitive? (car expr))))

(define (compile-primcall expr)
  (let ([prim (car expr)]
	[args (cdr expr)])
    (check-primcall-args prim args)
    (apply (primitive-emitter prim) args)))

(define (check-primcall-args prim args)
  (or (eq? (getprop prim '*arg-count*) (length args))
      (error "Wrong number of args" args)))


(define-primitive (fxadd1 arg)
  ;; add 1 to arg
  ;; 1 needs to be converted to ptr representation
  ;; results needs to be in ptr representation
  (emit " %v~d = add i32 ~a, ~a" (gensym) (immediate-rep 1) (compile-expr arg))
  (format "%v~d" symcnt))

(define-primitive (char->fixnum arg)
  ;; %1 = lshr i32 arg, 6
  (emit " %v~d = lshr i32 ~a, 6" (gensym) (compile-expr arg))
  (format "%v~d" symcnt))

(define-primitive (fixnum->char arg)
  ;; %1 = shl i32 arg, 6
  ;; %2 = or i32 %1, chartag
  (let ([v1 (gensym)]
	[v2 (gensym)])
    (emit " %v~d = shl i32 ~a, 6" v1 (compile-expr arg))
  (emit " %v~d = or i32 %v~d, ~d" v2 v1 chartag)
  (format "%v~d" v2)))

(define-primitive (fixnum? arg)
  ;; %1 = and i32 arg, fxmask
  ;; %2 = icmp eq i32 %1, fxtag
  ;; %3 = select i1 %2, i32 bool_t, i32 bool_f
  (let ([v1 (gensym)]
	[v2 (gensym)]
	[v3 (gensym)])
    (emit " %v~d = and i32 ~a, ~d" v1 (compile-expr arg) fxmask)
    (emit " %v~d = icmp eq i32 %v~d, ~d" v2 v1 fxtag)
    (emit " %v~d = select i1 %v~d, i32 ~d, i32 ~d" v3 v2 bool_t bool_f)
    (format "%v~d" v3)))


(define (compile-expr expr)
  (cond
   [(immediate? expr) (compile-immediate expr)]
   [(primcall? expr) (compile-primcall expr)] 
   [else (error "Neither immediate nor primcall" expr)]))


(define (compile-program x)
  (emit "define i32 @scheme_entry(){")
  (emit " ret i32 ~a" (compile-expr x))
  (emit "}"))

(load "tests-driver.scm")
;(load "tests-1.1-req.scm")
;(load "tests-1.2-req.scm")
(load "tests-1.3-req.scm")
