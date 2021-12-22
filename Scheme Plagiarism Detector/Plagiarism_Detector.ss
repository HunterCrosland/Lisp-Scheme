#lang racket




; check if value is a lambda expr
(define (lambda? a)
  (member a '(lambda λ)))

(define (if? a)
  (and (equal? (list a) 'if))
)

;check if value is a quote
(define (quote? a)
  (equal? a 'quote))

(define (second a) (car (cdr a)))

;wanted a short xor
;from https://stackoverflow.com/questions/1930901/exclusive-or-in-scheme
(define (xor a b)
  (not (boolean=? a b)))


(define (expr-compare a b)
    ;(display a) (display ", ") (display b) (newline)
    (cond
        
        [(equal? a b) a]
        
        [(and (boolean? a) (boolean? a)) (if a '% '(not %))]
       
        [(or (not (list? a)) (not (list? b)))
         (list 'if '% a b)]
        
        [(not (equal? (length a) (length b))) (list 'if '% a b)]
       
        [else (list-compare a b)]
    )
)

(define (list-compare a b)
    (cond
        
        [(or (quote? (car a)) (quote? (car b))) (list 'if '% a b)]
        ; case where lists are a special form if expression
        [(and (if? (car a)) (if? (car b))) 
        (cons (expr-compare (car a) (car b)) (expr-compare (cdr a) (cdr b)))]
        ; case where one is an if and another is not
        [(and (or (if? (car a)) (if? (car b))) (or (equal? (length a) 4) (equal? (length b) 4))) 
        (display "HERE ") (newline)
        `(if '% ,a ,b)]
        ; case where lists are a special form lambda expression
        [(and (lambda? (car a)) (lambda? (car b))) 
          (if (and (equal? (length a) 3) (list? (car (cdr a))))
            (lambda-compare a b)
            (cons (expr-compare (car a) (car b)) (expr-compare (cdr a) (cdr b))))
        ]
        ; case where one expression is a proper lambda function and other is not
        [(or (and (lambda? (car a)) (equal? (length a) 3)) (and (lambda? (car b)) (equal? (length b) 3))) (list 'if '% a b)]
        ; all other cases
        [else (cons (expr-compare (car a) (car b)) (expr-compare (cdr a) (cdr b)))]
    )
)



(define (lambda-compare a b)
    (cond
    [(not (equal? (length (second a)) (length (second b)))) `(if % ,a ,b)]
    [(or (equal? (car a) 'λ) (equal? (car b) 'λ))
    (if 
    (equal? (second a) (second b)) 
    (cons 'λ (expr-compare (cdr a) (cdr b))) 
    (cons 'λ (lambda-hashmap-get-set (cdr a) (cdr b)))
    )]
    [(not (equal? (second a) (second b))) 
    (cons 'lambda (lambda-hashmap-get-set (cdr a) (cdr b)))]
    ;else they should be treated like unequal singletons
    [else 
    (cons 'lambda (expr-compare (cdr a) (cdr b))) ]
    )

)

; ; handle lambda case special form
; (define (lambda-compare a b)
;     (cond
;       ; if the two lambda functions have different arguments, don't combine
;       [(not (equal? (length (car (cdr a))) (length (car (cdr b))))) (list 'if '% a b)]
;       ; case where at least one of the lambdas is a λ
;       [(or (equal? (car a) 'λ) (equal? (car b) 'λ)) (cons 'λ (lambda-hashmap-get-set (cdr a) (cdr b)))]
;       ; case where neither are λ
;       [else (cons 'lambda (lambda-hashmap-get-set (cdr a) (cdr b)))]
;     )
; )


(define (lambda-hashmap-get-set a b)
  ; define the hashmaps for lambda expressions
  (define lambda-hashmap-x (make-weak-hash))
  (define lambda-hashmap-y (make-weak-hash))
  ; append list of formals to expr having properly hashed and replaced values
  (cons (lambda-arg-get-set (car a) (car b) lambda-hashmap-x lambda-hashmap-y) (expr-compare (lambda-fun-get (cdr a) lambda-hashmap-x) (lambda-fun-get (cdr b) lambda-hashmap-y)))
)

(define (lambda-arg-get-set a b my-a-hash my-b-hash) 

  (cond 
  [(equal? a b) a]
  [(or (null? a) (null? b)) null]
  [(and (list? a) (list? b)) (cons (lambda-arg-get-set (car a) (car b) my-a-hash my-b-hash) (lambda-arg-get-set (cdr a) (cdr b) my-a-hash my-b-hash))]
  [else (hash-set! my-a-hash a (string->symbol (apply string-append (map symbol->string `(,a ! ,b)))))
          (hash-set! my-b-hash b (string->symbol (apply string-append (map symbol->string `(,a ! ,b)))))
          (lambda-fun-get a my-a-hash)]
  )
)

(define (lambda-redefine-map a my-a-hash)
(cond
        [(null? a) my-a-hash]
        [(list? (car a)) (lambda-redefine-map (car a) my-a-hash)]
        
        [else 
        (hash-set! my-a-hash (car a) (car a)) 
        (lambda-redefine-map (cdr a) my-a-hash)
        ]
      )
)

(define (lambda-fun-get a my-a-hash)

  (cond
  [(null? a) null]
  [(symbol? a)
  (hash-ref my-a-hash a a)]
  [(and (list? a) (equal? (length a) 4) (if? (car a))) (cons 'if (lambda-fun-get (cdr a) my-a-hash))]
  [(and (list? a) (equal? (length a) 3) (lambda? (car a)) (list? (car (cdr a)))) 
            ; create a new hashmap that is a copy of original but with nested lambda args rehashed
            (define lambda-hashmap2 (hash-copy my-a-hash)) 
            (cons (lambda-fun-get (car a) my-a-hash) (lambda-fun-get (cdr a) (lambda-redefine-map (cdr a) lambda-hashmap2)))
          ]
  
  [(list? a)
  (cons (lambda-fun-get (car a) my-a-hash) (lambda-fun-get (cdr a) my-a-hash))]
  [else (hash-ref my-a-hash a a)]
  )


)








; ((+ lambda!if if (f lambda!if)))
; ((+ lambda!if lambda!if (f λ)))
;correct:  (+ lambda!if (if % if lambda!if) (f (if % lambda!if λ)))
;mine:     (+ lambda!if if % (if (f lambda!if)) (lambda!if (f λ)))


; '((lambda (lambda!if) ) 3))

(define ns-eval
  (let ((ns (make-base-namespace)))
    (lambda (expr) (eval expr ns))))


(define (test-expr-compare a b) 
  (display "inside: ") (newline)
    (let ([w (equal? (ns-eval `(let ([% #t]) ,(expr-compare a b))) (ns-eval a))]) 
    (let ([v (equal? (ns-eval `(let ([% #f]) ,(expr-compare a b))) (ns-eval b))])
    (and w v)))
)

; (display "test-expr-compare: ") (newline)
; (test-expr-compare '(cons 1 2) '(cons 3 4))


(expr-compare '(if x y z) '(g x y z))
(display "'(if % (if x y z) (g x y z))")

(display "1 ") (equal? (expr-compare 12 12) '12)
(equal? (expr-compare 12 20) '(if % 12 20))
(equal? (expr-compare #t #t) #t)
(equal? (expr-compare #f #f) #f)
(display "5 ") (equal? (expr-compare #t #f) '%)
(equal? (expr-compare #f #t) '(not %))
(equal? (expr-compare 'a '(cons a b)) '(if % a (cons a b)))
(equal? (expr-compare '(cons a b) '(cons a b)) '(cons a b))
(equal? (expr-compare '(cons a lambda) '(cons a λ)) '(cons a (if % lambda λ)))
(display "10 ") (equal? (expr-compare '(cons (cons a b) (cons b c))
              '(cons (cons a c) (cons a c))) '(cons (cons a (if % b c)) (cons (if % b a) c)))
(equal? (expr-compare '(cons a b) '(list a b)) '((if % cons list) a b))
(equal? (expr-compare '(list) '(list a)) '(if % (list) (list a)))
(equal? (expr-compare ''(a b) ''(a c)) '(if % '(a b) '(a c)))
(equal? (expr-compare '(quote (a b)) '(quote (a c))) '(if % '(a b) '(a c)))
(display "15 ") (equal? (expr-compare '(quoth (a b)) '(quoth (a c))) '(quoth (a (if % b c))))
(equal? (expr-compare '(if x y z) '(if x z z)) '(if x (if % y z) z))
(equal? (expr-compare '(if x y z) '(g x y z)) '(if % (if x y z) (g x y z)))
(equal? (expr-compare '((lambda (a) (f a)) 1) '((lambda (a) (g a)) 2)) '((lambda (a) ((if % f g) a)) (if % 1 2)))
(equal? (expr-compare '((lambda (a) (f a)) 1) '((λ (a) (g a)) 2)) '((λ (a) ((if % f g) a)) (if % 1 2)))
(display "20 ") (equal? (expr-compare '((lambda (a) a) c) '((lambda (b) b) d)) '((lambda (a!b) a!b) (if % c d)))
(equal? (expr-compare ''((λ (a) a) c) ''((lambda (b) b) d)) '(if % '((λ (a) a) c) '((lambda (b) b) d)))
(equal? (expr-compare '(+ #f ((λ (a b) (f a b)) 1 2))
              '(+ #t ((lambda (a c) (f a c)) 1 2))) '(+
     (not %)
     ((λ (a b!c) (f a b!c)) 1 2)))
(equal? (expr-compare '((λ (a b) (f a b)) 1 2)
              '((λ (a b) (f b a)) 1 2)) '((λ (a b) (f (if % a b) (if % b a))) 1 2))
(equal? (expr-compare '((λ (a b) (f a b)) 1 2)
              '((λ (a c) (f c a)) 1 2)) '((λ (a b!c) (f (if % a b!c) (if % b!c a))) 1 2))

(display "25 ") (equal? (expr-compare '((lambda (lambda) (+ lambda if (f lambda))) 3)
              '((lambda (if) (+ if if (f λ))) 3)) '((lambda (lambda!if) (+ lambda!if (if % if lambda!if) (f (if % lambda!if λ)))) 3))
(equal? (expr-compare '((lambda (a) (eq? a ((λ (a b) ((λ (a b) (a b)) b a))
                                    a (lambda (a) a))))
                (lambda (b a) (b a)))
              '((λ (a) (eqv? a ((lambda (b a) ((lambda (a b) (a b)) b a))
                                a (λ (b) a))))
                (lambda (a b) (a b)))) '((λ (a)
      ((if % eq? eqv?)
       a
       ((λ (a!b b!a) ((λ (a b) (a b)) (if % b!a a!b) (if % a!b b!a)))
        a (λ (a!b) (if % a!b a)))))
     (lambda (b!a a!b) (b!a a!b))))

(equal? (expr-compare '(cons a lambda) '(cons a λ))
      '(cons a (if % lambda λ)))
(equal? (expr-compare '(lambda (a) a) '(lambda (b) b))
       '(lambda (a!b) a!b))
(equal? (expr-compare '(lambda (a) b) '(cons (c) b))
      '(if % (lambda (a) b) (cons (c) b)))
(display "30 ") (equal? (expr-compare '((λ (if) (+ if 1)) 3) '((lambda (fi) (+ fi 1)) 3))
      '((λ (if!fi) (+ if!fi 1)) 3))
(equal? (expr-compare '(lambda (lambda) lambda) '(λ (λ) λ))
       '(λ (lambda!λ) lambda!λ))
(equal? (expr-compare ''lambda '(quote λ))
      '(if % 'lambda 'λ))
(equal? (expr-compare '(lambda (a b) a) '(λ (b) b))
      '(if % (lambda (a b) a) (λ (b) b)))
(equal? (expr-compare '(λ (a b) (lambda (b) b)) '(lambda (b) (λ (b) b)))
       '(if % (λ (a b) (lambda (b) b)) (lambda (b) (λ (b) b))))
(display "35 ") (equal? (expr-compare '(λ (let) (let ((x 1)) x)) '(lambda (let) (let ((y 1)) y)))
       '(λ (let) (let (((if % x y) 1)) (if % x y))))
(equal? (expr-compare '(λ (x) ((λ (x) x) x))
              '(λ (y) ((λ (x) y) x)))
     '(λ (x!y) ((λ (x) (if % x x!y)) (if % x!y x))))
(equal? (expr-compare '(((λ (g)
                   ((λ (x) (g (λ () (x x))))     ; This is the way we define a recursive function
                    (λ (x) (g (λ () (x x))))))   ; when we don't have 'letrec'
                 (λ (r)                               ; Here (r) will be the function itself
                   (λ (n) (if (= n 0)
                              1
                              (* n ((r) (- n 1))))))) ; Therefore this thing calculates factorial of n
                10)
              '(((λ (x)
                   ((λ (n) (x (λ () (n n))))
                    (λ (r) (x (λ () (r r))))))
                 (λ (g)
                   (λ (x) (if (= x 0)
                              1
                              (* x ((g) (- x 1)))))))
                9))
      '(((λ (g!x)
                  ((λ (x!n) (g!x (λ () (x!n x!n))))
                   (λ (x!r) (g!x (λ () (x!r x!r))))))
                (λ (r!g)
                  (λ (n!x) (if (= n!x 0)
                               1
                               (* n!x ((r!g) (- n!x 1)))))))
               (if % 10 9)))