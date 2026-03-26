#lang racket
(module+ test
  (require rackunit))

(define *rule-base*
  '((rule1 (and (USA) (English)) --> (Honolulu))
    (rule2 (and (Europe) (France)) --> (Paris))
    (rule3 (and (USA) (Continent)) --> (LosAnge))
    (rule4 (and (Island) (Equator)) --> (Honolulu))
    (rule5 (and (Asia) (Equator)) --> (Singapore))
    (rule6 (and (Island) (Micronesia)) --> (Guam))
    (rule7 (Swimming) --> (Equator))))

(define *working-memory* '((Island) (Swiming)))

(define (get-rulename rule) (car rule))
(define (get-cond rule) (cadr rule))
(define (get-action rule) (cadddr rule))

(define (empty? a) (null? a))

(define (element? x a)
  (cond ((empty? a) #t)
        ((equal? x (car a)) #t)
        (else (element? x (cdr a)))))

(define (condition-aux? conds states)
  (cond ((null? conds) #t)
        ((element? (car conds) states)
         (condition-aux? (cdr conds) states))
        (else #f)))

(define (rule-cond? conds states)
  (cond ((null? conds) #t)
        ((eq? (car conds) 'and)
         (condition-aux? (cdr conds) states))
        (else (element? conds states))))

(define (pattern-matching states)
  (let ((enables '()))
    (for-each (lambda (candidate)
                (cond ([rule-cond? (get-cond candidate) states]
                       (set! enables (cons (get-rulename candidate) enables)))))
                  
              *rule-base*)
    enables))

(define (printn x . y)
  (display x)
  (cond ([not (null? y)]
         (for-each (lambda (z) (display z)) y)))
  (newline))

(define (choice lst)
  (cond ((null? list) '())
        (else (printn "enable rules : " lst)
              (display "enter rule-name >> ")
              (read))))

(define (get-rule r rules)
  (if (null? rules) '()
    (let ((rule (car rules)))
      (if (eq? (car rule) r)
        rule
        (get-rule r (cdr rules))))))

(define (eval-action action memory)
  (set! memory (cons action memory))
  (printn "action : " action)
  memory)

(define (rule-action! r memory)
  (let ((rule (get-rule r *rule-base*)))
    (if (null? rule) memory
      (eval-action (get-action rule) memory))))

(define (output-data memory)
  (printn " *working-memory : " memory))

(define (forward-reasoning memory)
  (do
    ([rule (choice (pattern-matching memory))
           (choice (pattern-matching memory))])
    [(or (null? rule)
         (eq? rule 'quit)) 'end]
    (set! memory (rule-action! rule memory))
    (output-data memory)))




(module+ test
  ;; Any code in this `test` submodule runs when this file is run using DrRacket
  ;; or with `raco test`. The code here does not run when this file is
  ;; required by another module.

  (check-equal? (+ 2 2) 4))

(module+ main
  ;; (Optional) main submodule. Put code here if you need it to be executed when
  ;; this file is run using DrRacket or the `racket` executable.  The code here
  ;; does not run when this file is required by another module. Documentation:
  ;; http://docs.racket-lang.org/guide/Module_Syntax.html#%28part._main-and-test%29
  (forward-reasoning *working-memory*))

#|
  (require racket/cmdline)
  (define who (box "world"))
  (command-line
    #:program "my-program"
    #:once-each
    [("-n" "--name") name "Who to say hello to" (set-box! who name)]
    #:args ()
    (printf "hello ~a~n" (unbox who))))
|#


