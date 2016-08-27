;dc
;-- = 0 当c是一个常量或是一个与x不同的变量
;dx
;===============================
;dx
;-- = 1
;dx
;===============================
;d(u + v)   du   dv
;-------- = -- + --
;   dx      dx   dx
;===============================
;d(uv)     dv     du
;----- = u*-- + v*--
; dx       dx     dx
;(variable? e)  e是变量吗？
;(same-variable? v1 v2)  v1和v2是同一个变量吗？
;(sum? e) e是一个和式
;(addend e) e的被加项
;(augend e) e的加项
;(make-sum a1 a2) 构造器a1和a2的和式
;(product? e) e是乘式吗
;(multiplier e) e的被乘式
;(multiplicand e) e的乘式
;(make-product m1 m2) 构造器m1 与 m2 的乘式
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp) (make-sum (make-product (multiplier exp) (deriv (multiplicand exp) var))
                                  (make-product (multiplicand exp) (deriv (multiplier exp) var))))
        ((exponentiation? exp)
            (let ((u (base exp))
                  (n (exponent exp)))
                 (make-product (make-product n
                                             (make-exponentiation u (- n 1)))
                               (deriv u var))))
        (else (error "unknow expression type -- DERIV" exp))))

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (sum? e)
  (and (pair? e) (eq? (car e) '+)))
(define (addend e)
  (cadr e))
(define (augend e)
  (let ((a (cddr e)))
       (if (= (length a) 1)
           (car a)
           (make-sum-list a))))
(define (make-sum-list lst)
  (if (= (length lst) 2)
      (make-sum (car lst) (cadr lst))
      (make-sum (car lst) (make-sum-list (cdr lst)))))
;(define (make-sum e1 e2)
;  (list '+ e1 e2))
(define (product? e)
  (and (pair? e) (eq? (car e) '*)))
(define (multiplier e)
  (cadr e))
(define (multiplicand e)
  (let ((a (cddr e)))
       (if (= (length a) 1)
           (car a)
           (make-product-list a))))

(define (make-sum e1 e2)
  (cond ((=number? e1 0) e2)
        ((=number? e2 0) e1)
        ((and (number? e1) (number? e2)) (+ e1 e2))
        (else (list '+ e1 e2))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-product e1 e2)
  (cond ((or (=number? e1 0) (=number? e2 0)) 0)
        ((=number? e1 1) e2)
        ((=number? e2 1) e1)
        ((and (number? e1) (number? e2)) (* e1 e2))
        (else (list '* e1 e2))))
(define (make-product-list lst)
  (if (= (length lst) 2)
      (make-product (car lst) (cadr lst))
      (else (make-product (car lst) (make-product-list (cdr lst))))))
(define (exponentiation? e)
  (eq? (car e) '**))
(define (base e)
  (cadr e))
(define (exponent e)
  (caddr e))
(define (make-exponentiation e1 e2)
  (cond ((=number? e2 0) 1)
        ((=number? e2 1) e1)
        (else (list '** e1 e2))))


;(deriv '(+ x 3) 'x)
;(+ 1 0)
;(deriv '(* x y) 'x)
;(+ (* x 0) (* y 1))
;(deriv '(* (* x y) (+ x 3)) 'x)
(deriv '(* x y (+ x 3)) 'x)
