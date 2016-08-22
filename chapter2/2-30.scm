(define (square-tree tree)
  (cond ((null? tree) (quote ()))
        ((atom? (car tree)) (cons (square (car tree)) (square-tree (cdr tree))))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))
(define square-tree-map
    (lambda (tree)
      (map (lambda (sub-tree)
                (cond ((pair? sub-tree) (square-tree-map sub-tree))
                      (else (square sub-tree))))
           tree)))
(square-tree-map (list (list 1 (list 2) (list 2) 4 (list 3))))