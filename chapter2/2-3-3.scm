;(define (element-of-set? x set)
;  (cond ((null? set) #f)
;        ((equal? x (car set)) #t)
;        (else (element-of-set? x (cdr set)))))
;(define (element-of-set? x set)
;  (cond ((null? set) #f)
;        ((= x (car set)) #t)
;        ((< x (car set)) #f)
;        (else (element-of-set? x (cdr set)))))
(define set?
    (lambda (set)
      (cond ((null? set) #t)
            (else (and (not (element-of-set? (car set) (cdr set)))
                       (set? (cdr set)))))))
(define make-set
    (lambda (lst)
      (cond ((null? lst) (quote ()))
            (else (cons (car lst)
                        (make-set (remove (car lst) (cdr lst))))))))
;(define (adjoin x set)
;  (cond ((element-of-set? x set) set)
;        (else (cons x set))))
;;基于排序的集合
;(define (adjoin x set)
;  (cond ((null? set) (list x))
;        ((= x (car set)) set)
;        ((< x (car set)) (cons x set))
;        ((> x (car set)) (cons (car set) (adjoin x (cdr set))))))
;交集
;(define (intersection-set set1 set2)
;  (cond ((or (null? set1) (null? set2)) (quote ()))
;        (else (cond ((element-of-set? (car set1) set2)
;                     (cons (car set1) (intersection-set (cdr set1) set2)))
;                    (else (intersection-set (cdr set1) set2))))))
;
(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      (quote ())
      (let ((a1 (car set1))
            (a2 (car set2)))
           (cond ((= a1 a2) (cons a1 (intersection-set (cdr set1) (cdr set2))))
                 ((< a1 a2) (intersection-set (cdr set1) set2))
                 ((> a1 a2) (intersection-set set1 (cdr set2)))))))
;;并集
;(define (union-set set1 set2)
;  (cond ((null? set1) set2)
;        ((null? set2) set1)
;        ((element-of-set? (car set1) set2) (union-set (cdr set1) set2))
;        (else (cons (car set1) (union-set (cdr set1) set2)))))
;基于排序集合的并集
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else (let ((a1 (car set1))
                    (a2 (car set2)))
                   (cond ((= a1 a2) (cons a1 (union-set (cdr set1) (cdr set2))))
                         ((< a1 a2) (cons a1 (union-set (cdr set1) set2)))
                         ((> a1 a2) (cons a2 (union-set set1 (cdr set2)))))))))
;=================================================
;集合作为二叉树
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))
(define (element-of-set x tree)
  (cond ((null? set) #f)
        ((= x (entry tree)) #t)
        ((< x (entry tree)) (element-of-set x (left-branch tree)))
        ((> x (entry tree)) (element-of-set x (right-branch tree)))))
(define (adjoin x tree)
  (cond ((null? tree) (make-tree x (quote ()) (quote ())))
        ((= x (entry tree)) tree)
        ((< x (entry tree)) (make-tree (entry tree)
                                       (adjoin x (left-branch tree))
                                       (right-branch tree)))
        ((> x (entry tree)) (make-tree (entry tree)
                                       (left-branch tree)
                                       (adjoin x (right-branch tree))))))




(define equal?
    (lambda (e1 e2)
      (cond ((and (atom? e1) (atom? e2)) (eq-atom? e1 e2))
            ((or (atom? e1) (atom? e2)) #f)
            (else (eq-list? e1 e2)))))
(define remove
    (lambda (a lst)
      (cond ((null? lst) (quote ()))
            ((equal? a (car lst)) (remove a (cdr lst)))
            (else (cons (car lst) (remove a (cdr lst)))))))
(define eq-list?
    (lambda (lst1 lst2)
      (cond ((and (null? lst1) (null? lst2)) #t)
            ((or (null? lst1) (null? lst2)) #f)
            (else (and (equal? (car lst1) (car lst2)) (eq-list? (cdr lst1) (cdr lst2)))))))
(define eq-atom?
    (lambda (a1 a2)
      (cond ((and (number? a1) (number? a2)) (= a1 a2))
            ((or (number? a1) (number? a2)) #f)
            (else (eq? a1 a2)))))
(define atom?
    (lambda (x)
      (and (not (null? x)) (not (pair? x)))))
(union-set (list 1 2 3 4) (list 3 4 5 6))
