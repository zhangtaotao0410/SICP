(load "2-3-3.scm")
(define (tree-to-list-1 tree)
  (cond ((null? tree) (quote ()))
        (else (append (tree-to-list-1 (left-branch tree))
                      (cons (entry tree)
                            (tree-to-list-1 (right-branch tree)))))))
(define (tree-to-list-2 tree)
  (define (copy-to-list tree result)
    (cond ((null? tree) result)
          (else (copy-to-list (left-branch tree)
                              (cons (entry tree)
                                    (copy-to-list (right-branch tree) result))))))
  (copy-to-list tree '()))
  ;
  ;(define (make-tree entry left right)
  ;  (list entry left right))

  (define a (make-tree 7
                           (make-tree 3
                                      (make-tree 1 '() '())
                                      (make-tree 5 '() '()))
                           (make-tree 9
                                      '()
                                      (make-tree 11 '() '()))))
(tree-to-list-2 a)
