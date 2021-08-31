#lang nanopass

(define-language L0
  (terminals
   (variable (x))
   (primitive (pr))
   (constant (c)))
  (Expr (e body)
        x
        pr
        c
        (begin e* ... e)
        (if e0 e1)
        (if e0 e1 e2)
        (lambda (x* ...) body* ... body)
        (let ([x* e*] ...) body* ... body)
        (letrec ([x* e*] ...) body* ... body)
        (e0 e1 ...)
        (reset e)
        (shift x e)))

(define variable?
  (lambda (x)
    (symbol? x)))

(define primitive?
  (lambda (x)
    (memq x '(+ displayln void vector vector-ref))))

(define constant?
  (lambda (x)
    (or (number? x)
        (char? x)
        (string? x))))

(define-language L1
  (extends L0)
  (Expr (e body)
        (-
           (if e0 e1)
           (lambda (x* ...) body* ... body)
           (let ([x* e*] ...) body* ... body)
           (letrec ([x* e*] ...) body* ... body))
        (+ (lambda (x* ...) body)
           (let ([x* e*] ...) body)
           (letrec ([x* e*] ...) body))))


(define-pass make-explicit : L0 (ir) -> L1 ()
  (definitions)
  (Expr : Expr (ir) -> Expr ()
        [(if ,[e0] ,[e1]) `(if ,e0 ,e1 (void))]
        [(lambda (,x* ...) ,[body*] ... ,[body])
         `(lambda (,x* ...) (begin ,body* ... body))]
        [(let ([,x* ,[e*]] ...) ,[body*] ... ,[body])
         `(let ([,x* ,e*] ...) (begin ,body* ... ,body))]
        [(letrec ([,x* ,[e*]] ...) ,[body*] ... ,[body])
         `(letrec ([,x* ,e*] ...) (begin ,body* ... ,body))]))

; Remove letrecs

(define-language L2
  (extends L1)
  (Expr (e body)
        (-
         (letrec ([x* e*] ...) body))
        (+
         (set! x e))))
; See page 2:
; https://guenchi.github.io/Scheme/doc/Fixing%20Letrec%20A%20Faithful%20Yet%20Efficient%20Implementation%20of%20Scheme%E2%80%99s%20Recursive%20Binding%20Construct.pdf
#|
(define-pass no-letrecs : L1 (ir) -> L2 ()
  (definitions)
  (RemLetRec : Expr (ir) -> Expr ()
             [(letrec ([,x* ,e*] ...) ,[body])
              `(let ([,x* ,e*] ...)
                 )]))
|#

; Single-argument lambda/lets only

(define-language L3
  (extends L2)
  (Expr (e body)
        (-
         (let ([x* e*] ...) body)
         (lambda (x* ...) body))
        (+
         (let ([x e]) body)
         (lambda (x) body))))

(define-pass single-bindings : L1 (ir) -> L2 ()
  (definitions)
  (Singlify : Expr (ir) -> Expr ()
            [(reset ,[e])
             `(reset ,e)]
            [(shift ,x ,[e])
             `(shift ,x ,[e])]
            [(if ,[e0] ,[e1] ,[e2])
             `(if ,e0 ,e1 ,e2)]

            [(lambda (,x) ,[body])
             `(lambda (,x) ,body)]
            [(lambda (,x0 ,x1 ,x* ...) ,body)
             `(lambda (,x0)
                ,(Singlify
                  (with-output-language (L1 Expr)
                    `(lambda (,x1 ,x* ...)
                       ,body))))]

            [(let ([,x ,[e]]) ,[body])
             `(let ([,x ,e])
                ,body)]
            ))


; No lets, only lambda

(define-language L3
  (extends L2)
  (Expr (e body)
        (- (let ([x e]) body))))

; Closure conversion

; CPS

; Output to JS?
