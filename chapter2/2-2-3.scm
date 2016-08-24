(define sum-odd-square
    (lambda (tree)
      (cond ((null? tree) 0)
            ((atom? (car tree)) (if (odd? (car tree))
                                    (+ (square (car tree)) (sum-odd-square (cdr tree)))
                                    (sum-odd-square (cdr tree))))

            (else (+ (sum-odd-square (car tree))
                     (sum-odd-square (cdr tree)))))))

;映射过程
(define map
    (lambda (pro l)
      (cond ((null? l) (quote ()))
            (else (cons (pro (car l)) (map pro (cdr l)))))))
;过滤过程
(define filter
    (lambda (predicate l)
        (cond ((null? l) (quote ()))
              (else (cond ((predicate (car l)) (cons (car l) (filter predicate (cdr l))))
                          (else (filter predicate (cdr l))))))))
;累计过程
(define (accumulate op initial lst)
  (cond ((null? lst) initial)
        (else (op (car lst) (accumulate op initial (cdr lst))))))
(define atom?
    (lambda (x)
      (and (not (null? x)) (not (pair? x)))))
;枚举区间的数
(define (enumerate-interval low high)
  (cond ((> low high) (quote ()))
        (else (cons low (enumerate-interval (add1 low) high)))))
;枚举tree的所有叶子
(define (enumerate-tree tree)
  (cond ((null? tree) (quote ()))
        ((atom? (car tree)) (cons (car tree) (enumerate-tree (cdr tree))))
        (else (append (enumerate-tree (car tree)) (enumerate-tree (cdr tree))))))

(define (sum-odd-square tree)
  (accumulate +
              0
              (map square
                   (filter odd?
                           (enumerate-tree tree)))))
;判断是否为素数
(define (prime? n)
  (= n (smallest-divisor n)))
(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (* 2 test-divisor) n) n)
        ((divides? n test-divisor) test-divisor)
        (else (find-divisor n (add1 test-divisor)))))
(define (divides? n m)
  (= (remainder n m) 0))
(define add1
  (lambda (n)
    (+ n 1)))

(accumulate append (quote ()) (map (lambda (x) (map (lambda (y) (list x y (+ x y)))
                      (filter (lambda (a) (prime? (+ a x)))
                              (enumerate-interval 1 (- x 1)))))
     (enumerate-interval 1 6)))

;
;(accumulate append
;            (quote ())
;            (map (lambda (x)
;                         (map (lambda (y) (list x y))
;                              (enumerate-interval 1 (- x 1))))
;                 (enumerate-interval 1 6)))
(define (flatmap pro seqs)
  (accumulate append (quote ()) (map pro seqs)))

(define prime-sum?
    (lambda (pair)
      (prime? (+ (car pair) (cadr pair)))))
(define make-pair-sum
    (lambda (pair)
      (list (car pair) (cadr pair) (+ (car pair) (cadr pair)))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap (lambda (x) (map (lambda (y) (list x y))
                                         (enumerate-interval 1 (- x 1))))
                        (enumerate-interval 1 n)))))
(prime-sum-pairs 6)

(define (permutations s)
  (if (null? s)
      (list (quote ()))
      (flatmap (lambda (x)
                       (map (lambda (p) (cons x p))
                            (permutations (remove x s))))
               s)))
(define (remove x s)
  (filter (lambda (a)
                  (not (= x a)))
          s))
(permutations (list 1 2 3))
;(enumerate-tree (list 1 2 3 (list (list 1 (list 1 2) (list 1 2) 3 4) 1 2)))
