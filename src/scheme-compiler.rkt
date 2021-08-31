#lang nanopass

(define-language Scheme
  (terminals
   (Variable (x))
   (Primitive (pr))
   (Constant (c)))
  (Expr (e body)
        x
        pr
        c
        (begin e* ... e)
        (if e0 e1 e2)
        (lambda (x* ...) body)
        (let ([x* e*] ...) body)
        (e0 e1 ...)
        (reset e)
        (shift x e)))

(define (Variable? x) (symbol? x))
(define (Primitive? x)
  (memq x
        '(+ - * = < display call/cc set!)))
(define (Constant? x)
  (number? x))
(define-parser parse-Scheme Scheme)

(define-language ClosureConvertedScheme
  (extends Scheme)
  (terminals
   (+
    (Self (self))))
  (Expr (e body)
        (- (lambda (x* ...) body))
        (+
         self
         (lambda (self x* ...) body)
         (vector-ref x c)
         (vector e* ...))))

(define Self
  (let ()
    (struct self-val ())
    (self-val)))
(define (Self? x)
  (eq? x Self))

; Actually should be done on CPS-converted Scheme
(define-pass closure-convert : Scheme (ir) -> ClosureConvertedScheme ()
  (definitions
    (define (env)
      '(+ - * = < display call/cc set!))
    (define (extend-env env x)
      (cons env x))
    (define (many-extend env xs)
      (remove-duplicates (foldl extend-env env xs)))
    (define in-env? member)
    (define (free-variables expr seen)
      (nanopass-case (Scheme Expr) expr
       [(lambda (,x* ...) ,body)
        (free-variables body (many-extend seen x*))]
       [(if ,e0 ,e1 ,e2)
        (remove-duplicates
         (append (free-variables e0 seen)
                 (free-variables e1 seen)
                 (free-variables e2 seen)))]
       [(let ([,x* ,e*] ...) ,body)
        (free-variables body (many-extend seen x*))]
       [(begin ,e* ... ,e)
        (remove-duplicates
         (append
          (free-variables e seen)
          (foldl append '()
                 (map (lambda (x) (free-variables x seen)) e*))))]
       [,x (if (in-env? x seen) '() (list x))]
       [,c '()]
       [(reset ,e) (free-variables e seen)]
       [(shift ,x ,e)
        (free-variables (many-extend seen (list x)) e)]
       [(,e0 ,e1 ...)
        (remove-duplicates
         (append (free-variables e0 seen))
         (foldl append '()
                (map (lambda (x) (free-variables x seen))
                     e1)))]
       )))

  (Expr : Expr (ir fvs) -> Expr ()
        [,x (guard (in-env? x fvs))
            `(vector-ref ,Self ,(index-of x fvs))]
        [,x (guard (not (in-env? x fvs)))
            x]
        [,pr pr]
        [,c c]
        [(begin ,e* ... ,e)
         `(begin ,(map (lambda (x) (closure-convert x fvs)) e*) ...
                 ,(closure-convert e fvs))]
        [(if ,e0 ,e1 ,e2)
         `(if ,(closure-convert e0 fvs)
              ,(closure-convert e1 fvs)
              ,(closure-convert e2 fvs))]
        [(lambda (,x* ...) ,body)
         (let* ([env~ (many-extend (env) x*)]
                [fvs (free-variables body env~)])
           `(vector (lambda (,Self ,x* ...) ,(closure-convert body fvs)) ; Pass in fvs to catamorphism in body
                    ,fvs ...))]
        [(let ([,x* ,e*] ...) ,body)
         (let* ([env~ (many-extend (env) x*)]
               [fvs (free-variables body env~)])
             `(let ([,x* ,(map (lambda (x) (closure-convert x fvs)) e*)] ...)
                ,(closure-convert body fvs)))]
        [((lambda (,x) ,body) ,e1 ...)
         `(,(closure-convert `(lambda (,x) ,body) fvs) ,(map (lambda (x) (closure-convert x fvs)) e1) ...)]
        [(,e ,e1 ...) ; Really ,x
         `((vector-ref ,e 0) ,e ,(map (lambda (x) (closure-convert x fvs)) e1) ...)]
        [(reset ,e)
         `(reset ,(closure-convert e fvs))]
        [(shift ,x ,e)
         (let* ([env~ (many-extend (env) (list x))]
                [fvs (free-variables e env~)])
           `(shift ,x ,(closure-convert e fvs)))])
  (Expr ir (env)))
;(free-variables (parse-Scheme '(let ([x 0]) (+ x y z))) (env))
;(free-variables (parse-Scheme '(lambda (x) (+ x y z))) (env))
;(free-variables (parse-Scheme '(lambda (x y z) (+ x y z))) (env))
;(free-variables (parse-Scheme '((lambda (x y z) (+ x y z)) 1 2 3)) (env))

