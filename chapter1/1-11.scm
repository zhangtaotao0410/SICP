(define (F n)
  (cond ((< n 3) n)
        (else (+ (F (- n 1))
                 (* 2 (F (- n 2)))
                 (* 3 (F (- n 3)))))))

(define (FF n)
  (FF-item 0 1 2 n))

(define FF-item
    (lambda (a b c count)
        (cond ((= count 0) a)
              (else (FF-item b
                             c
                             (+ c (* 2 b) (* 3 a))
                             (- count 1))))))

(FF 6)
