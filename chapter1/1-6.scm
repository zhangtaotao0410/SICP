(define (new-if predicate then-clause else-clause)
        (cond (predicate then-clause)
              (else else-clause)))


(define (sqrt-item guess x)
        (new-if (good-enough? guess x)
                x
                (sqrt-item (improve guess x) x)))

;非尾递归造成栈溢出
