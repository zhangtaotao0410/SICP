(define (map pro lst)
  (accumulate (lambda (x y)
                      (cons (pro x) y))
              (quote ())
              lst))
(define (accumulate op initial lst)
  (cond ((null? lst) initial)
        (else (op (car lst) (accumulate op initial (cdr lst))))))

(define (append l1 l2)
  (accumulate cons l2 l1))
(define (length l1)
  (accumulate (lambda (x y) (+ 1 y)) 0 l1))

(define (count-leaves tree)
  (accumulate +
              0
              (map (lambda(x)
                          (if (pair? x)
                              (count-leaves x)
                              1))
                    tree)))

(count-leaves (list 1 2 (list 1 (list 1 (list 1 2) 2) 2)))
