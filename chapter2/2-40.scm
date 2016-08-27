(define (unique-pairs n)
  (flatmap (lambda (x)
                   (map (lambda (y) (list x y))
                        (enumerate-tree 1 (- x 1))))
           (enumerate-interval 1 n)))
(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (unique-pairs n))))
