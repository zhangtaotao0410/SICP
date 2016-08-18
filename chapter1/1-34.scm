(define f
    (lambda (g)
      (g 2)))

;(f square)= 4
;(f (lambda (z) (* z (+ z 1)))) =6
(f f)
((lambda (g) (g 2)) 2)
(2 2)

;The object 2 is not applicable.
