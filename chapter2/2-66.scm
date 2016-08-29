(define (lookup given-key tree)
  (cond ((null? tree) #f)
        (else (let ((tree-key (key (entry tree)))
                    (entry-tree (entry tree)))
                   (cond ((= given-key tree-key) entry-tree)
                         ((< given-key tree-key) (lookup given-key (left-branch tree)))
                         ((> given-key tree-key) (lookup given-key (right-branch tree))))))))

(define (key pair)
  (car pair))

(define (pair key value) (cons key value))
