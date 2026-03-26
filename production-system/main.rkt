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

(define *working-memory* '((Island) (Swimming)))

(define (get-rulename rule) (car rule))
(define (get-cond rule) (cadr rule))
(define (get-action rule) (cadddr rule))
(define (get-rule r rules)
  (if (null? rules) '()
    (let ((rule (car rules)))
      (if (eq? (car rule) r)
        rule
        (get-rule r (cdr rules))))))

(define (empty? a) (null? a))

(define (element? x a)
  (cond ((empty? a) #f)
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

(define (pattern-matching rules states)
  (let ((enables '()))
    (for-each (lambda (candidate)
                (cond ((rule-cond? (get-cond candidate) states)
                       (set! enables (cons (get-rulename candidate) enables)))))
                  
              rules)
    enables))

(define (printn x . y)
  (display x)
  (cond ((not (null? y))
         (for-each (lambda (z) (display z)) y)))
  (newline))

(define (choice lst)
  (cond ((null? lst) '())
        (else (printn "enable rules : " lst)
              (display "enter rule-name >> ")
              (read))))

(define (eval-action action memory)
  (set! memory (cons action memory))
  (printn "action : " action)
  memory)

(define (rule-action! r rules memory)
  (let ((rule (get-rule r rules)))
    (if (null? rule) memory
      (eval-action (get-action rule) memory))))

(define (output-data memory)
  (printn " *working-memory : " memory))

(define (forward-reasoning rules memory choice-rule)
  (do
    ((rule (choice-rule (pattern-matching rules memory))
           (choice-rule (pattern-matching rules memory))))
    ((or (null? rule)
         (eq? rule 'quit)) 'end)
    (set! memory (rule-action! rule rules memory))
    (output-data memory)))


(module+ test
  ;; Any code in this `test` submodule runs when this file is run using DrRacket
  ;; or with `raco test`. The code here does not run when this file is
  ;; required by another module.

  ;empty?
  (check-equal? (empty? '()) #t)
  (check-equal? (empty? '(1 2 3)) #f)
  ;element?
  (check-equal? (element? 1 '(1 2 3)) #t)
  (check-equal? (element? 10 '(1 2 3)) #f)
  (check-equal? (element? 'a '(1 2 a 3)) #t)
  (check-equal? (element? 'a '(1 2 3)) #f)

  ;get-rulename
  (check-equal? (get-rulename '(rule7 (Swimming) --> (Equator))) 'rule7)
  ;get-cond
  (check-equal? (get-cond '(rule7 (Swimming) --> (Equator))) '(Swimming))
  ;get-action
  (check-equal? (get-action '(rule7 (Swimming) --> (Equator))) '(Equator))

  ;get-rule
  ;condition-aux?  
  ;rule-cond
  ;pattern-matching
  ;rule-action!
  ;forward-reasoning
  (let ((rules 
          '((rule1 (and (USA) (English)) --> (Honolulu))
            (rule2 (and (Europe) (France)) --> (Paris))
            (rule3 (and (USA) (Continent)) --> (LosAnge))
            (rule4 (and (Island) (Equator)) --> (Honolulu))
            (rule5 (and (Asia) (Equator)) --> (Singapore))
            (rule6 (and (Island) (Micronesia)) --> (Guam))
            (rule7 (Swimming) --> (Equator))))
        (states1 '((Swimming) (USA) (English)))
        (states2 '((Island) (Swimming))))

    (check-equal? (get-rule 'rule1 rules) '(rule1 (and (USA) (English)) --> (Honolulu)))
    (check-equal? (get-rule 'rule7 rules) '(rule7 (Swimming) --> (Equator)))
    (check-equal? (get-rule 'rule10 rules) '())

    (check-equal? (condition-aux? (cdr (get-cond (list-ref rules 0))) states1) #t)
    (check-equal? (condition-aux? (cdr (get-cond (list-ref rules 2))) states1) #f)

    (check-equal? (rule-cond? (get-cond (list-ref rules 0)) states1) #t)
    (check-equal? (rule-cond? (get-cond (list-ref rules 6)) states1) #t)
    (check-equal? (rule-cond? (get-cond (list-ref rules 2)) states1) #f)

    (check-equal? (pattern-matching rules states1) '(rule7 rule1))
    
    (check-equal? 
      (rule-action! (get-rulename (list-ref rules 0)) rules states1) 
      '((Honolulu) (Swimming) (USA) (English)))

    (check-equal? (forward-reasoning rules states2 choice) 'end))

)
(module+ main
  ;; (Optional) main submodule. Put code here if you need it to be executed when
  ;; this file is run using DrRacket or the `racket` executable.  The code here
  ;; does not run when this file is required by another module. Documentation:
  ;; http://docs.racket-lang.org/guide/Module_Syntax.html#%28part._main-and-test%29
  (forward-reasoning *rule-base* *working-memory* choice))

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


