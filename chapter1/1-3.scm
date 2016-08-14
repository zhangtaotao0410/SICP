(define max-sum
    (lambda (n1 n2 n3)
        (cond ((>= n1 n2)
                  (cond ((>= n2 n3) (+ n1 n2))
                        (else (+ n1 n3))))
              (else (cond ((>= n1 n3) (+ n2 n1))
                          (else (+ n2 n3)))))))
(max-sum 1 2 3)
