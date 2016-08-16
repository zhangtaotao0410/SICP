(define inc
    (lambda (n)
      (+ n 1)))
(define dec
    (lambda (n)
      (- n 1)))

(define add
    (lambda (n m)
      (cond ((= n 0) m)
            (else (inc (add (dec n) m))))))
(add 4 5)
(inc (add 3 5))
(inc (inc (add 2 5)))
(inc (inc (inc (add 1 5))))
(inc (inc (inc (inc (add 0 5)))))
(inc (inc (inc (inc 5))))
(inc (inc (inc 6)))
(inc (inc 7))
(inc 8)
9
;以上是一个线性递归计算过程
(define add
    (lambda (n m)
      (cond ((= n 0) m)
            (else (add (dec n) (inc m))))))
(add 2 5)
(add 1 6)
(add 0 7)
7
;以上是一个线性迭代计算过程
