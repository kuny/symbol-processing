#lang racket

(module+ test
  (require rackunit))

;vect* : list? list? -> list/
(define-syntax vct*
  (syntax-rules ()
    [(mtx* x1 x2)
     (map * x1 x2)]))

;vect+ : list? -> list?
(define-syntax vct+
  (syntax-rules ()
    [(sum x)
     (apply + x)]))

;;percepon
;and-gate : list? list? -> number?
(define (and-gate x1 x2)
  (let* ([x (list x1 x2)]
         [w (list 0.5 0.5)]
         [tmp (+ (vct+ (vct* x w)) -0.7)])
    (if (<= tmp 0) 0 1)))

;nand-gate : list? list? -> number?
(define (nand-gate x1 x2)
  (let* ([x (list x1 x2)]
         [w (list -0.5 -0.5)]
         [tmp (+ (vct+ (vct* x w)) 0.7)])
    (if (<= tmp 0) 0 1)))

;or-gate : list? list? -> number?
(define (or-gate x1 x2)
  (let* ([x (list x1 x2)]
         [w (list 0.5 0.5)]
         [tmp (+ (vct+ (vct* x w)) -0.2)])
    (if (<= tmp 0) 0 1)))

;xor-gate : list? list? -> number?
(define (xor-gate x1 x2)
  (let ([s1 (nand-gate x1 x2)]
        [s2 (or-gate x1 x2)])
    (and-gate s1 s2)))



(module+ test

  ;vect* vect+
  (check-equal? (vct* '(1 2) '(3 4)) '(3 8))
  (check-equal? (vct* '(0 0) '(0.5 0.5)) '(0 0))
  (check-equal? (vct+ (vct* '(0 0) '(0.5 0.5))) 0)
  (check-equal? (vct+ (vct* '(1 0) '(0.5 0.5))) 0.5)
  (check-equal? (vct+ (vct* '(0 1) '(0.5 0.5))) 0.5)
  (check-equal? (vct+ (vct* '(1 1) '(0.5 0.5))) 1.0)

  ;and-gate
  (check-equal? (and-gate 0 0) 0)
  (check-equal? (and-gate 1 0) 0)
  (check-equal? (and-gate 0 1) 0)
  (check-equal? (and-gate 1 1) 1)

  ;nand-gate
  (check-equal? (nand-gate 0 0) 1)
  (check-equal? (nand-gate 1 0) 1)
  (check-equal? (nand-gate 0 1) 1)
  (check-equal? (nand-gate 1 1) 0)

  ;or-gate
  (check-equal? (or-gate 0 0) 0)
  (check-equal? (or-gate 1 0) 1)
  (check-equal? (or-gate 0 1) 1)
  (check-equal? (or-gate 1 1) 1)

  ;xor-gate
  (check-equal? (xor-gate 0 0) 0)
  (check-equal? (xor-gate 1 0) 1)
  (check-equal? (xor-gate 0 1) 1)
  (check-equal? (xor-gate 1 1) 0)

)
(module+ main
  ;; (Optional) main submodule. Put code here if you need it to be executed when
  ;; this file is run using DrRacket or the `racket` executable.  The code here
  ;; does not run when this file is required by another module. Documentation:
  ;; http://docs.racket-lang.org/guide/Module_Syntax.html#%28part._main-and-test%29

  (require racket/cmdline)
  (define who (box "world"))
  (command-line
    #:program "my-program"
    #:once-each
    [("-n" "--name") name "Who to say hello to" (set-box! who name)]
    #:args ()
    (printf "hello ~a~n" (unbox who)))
)

