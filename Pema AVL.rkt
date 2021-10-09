#lang scheme
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define lat?
  (lambda (l)
    (cond ((null? l) #t)
          ((atom? (car l))(lat? (cdr l)))
          (else #f))))

(define member?
  (lambda (a lat)
    (cond ((null? lat) #f)
          ((eq?  a (car lat)) #t)
          (else (member? a (cdr lat))))))

(define rember
  (lambda (a lat)
    (cond ((null? lat) '())
          ((eq? a (car lat)) (cdr lat) )
          (else (cons (car lat) (rember a (cdr lat)))))))

(define first
  (lambda (p)
    (car p)))

(define second
  (lambda (p)
    (car (cdr p))))

;(define firstChild
 ; (lambda (l)
  ;  (car (cdr l))))

(define firstChild
  (lambda (l)
   (cond ((terminalNode? l) '()) (else(car (cdr l))))))

;(define secondChild
 ; (lambda (l)
  ;  (car (cdr (cdr l)))))

(define secondChild
  (lambda (l)
    (cond ((terminalNode? l) '()) (else (car (cdr (cdr l)))))))

(define returnNode
  (lambda (s l)
    (cond ((null? l) '())
          ((= s (first l)) l)                                   
          (else (cond  ((terminalNode? l) '())
                 (else
                 (cond ((= s (first l)) l)
                        (else
                              (cond ((< s (first l)) (returnNode s (firstChild l)))
                                    (else (returnNode s (secondChild l))))))))))))

(define length
  (lambda (l)
    (cond ((null? l) 0)
          (else (add1 (length (cdr l)))))))

(define terminalNode?
  (lambda (l)
    (cond ((= 1 (length l)) #t)
          (else #f))))

(define height
  (lambda (l)
    (cond ((null? l) 0)
          (else (cond ((terminalNode? l) 1)
                      (else (cond ((>= (height (firstChild l)) (height(secondChild l))) (+ 1 (height(firstChild l))))
                                  (else (+ 1 (height (secondChild l)))))))))))

(define rightRotation
  (lambda (l)
    (cons (first (firstChild l)) (cons (firstChild(firstChild l)) (cons (cons (first l) (cons (secondChild (firstChild l)) (cons (secondChild l) '()))) '())))))
    ;(cons (car (firstChild l)) (cons (firstChild(firstChild l)) '()))))

(define leftRotation
  (lambda (l)
    (cons (first(secondChild l))
          (cons (cons (first l) (cons (firstChild l) (cons (firstChild (secondChild l)) '()))) (cons (secondChild (secondChild l)) '())))))

(define left-rightRotation
  (lambda (l)
    (rightRotation(cons (first l) (cons (leftRotation (firstChild l)) (cons (secondChild l) '()))))))

(define right-leftRotation
  (lambda (l)
    (leftRotation (cons (first l) (cons (firstChild l) (cons (rightRotation (secondChild l)) '()))))))

(define balanceTree
  (lambda (l)
    (cond ((terminalNode? l) l)
          ((= 2 (- (height(firstChild l)) (height(secondChild l))))
           (cond ((= 1 (- (height(firstChild(firstChild l))) (height (secondChild (firstChild l))))) (rightRotation l))
                 (else (left-rightRotation l))))
          ((= -2 (- (height(firstChild l)) (height(secondChild l))))
           (cond ((= -1 (- (height(firstChild(secondChild l))) (height(secondChild(secondChild l))))) (leftRotation l))
                 (else (right-leftRotation l))))
          (else l))))

(define hasLeftSubtree             ; is there a left subtree
  (lambda (l)
    (cond ((null? (car (cdr l))) #f)
          (else #t))))

(define hasRightSubtree            ; is there a right subtree
  (lambda (l)
    (cond ((null? (car (cdr (cdr l)))) #f)
          (else #t))))

(define addNode1
  (lambda (S l)
    (cons (first l)
          (cond
            ((terminalNode? l)
             (cond ((< S (first l)) (cons (cons S '()) (cons '() '())))
                   (else (cons '() (cons (cons S '()) '())))))
            (else
             (cond
               ((< S (first l))
                (cond
                  ((hasLeftSubtree l) (cons (balanceTree(addNode1 S (firstChild l))) (cons (secondChild l) '())) )
                  (else (cons (cons S '()) (cons (secondChild l) '())))))
               (else
                (cond
                  ((hasRightSubtree l) (cons (firstChild l) (cons (balanceTree (addNode1 S (secondChild l))) '())))
                  (else (cons (firstChild l) (cons (cons S '()) '())))))))))))

;;; test
(define addNode
  (lambda (S l)
          (cond
            ((terminalNode? l)
             (cond ((< S (first l)) (cons (first l) (cons (cons S '()) (cons '() '()))))
                   (else (cons (first l) (cons '() (cons (cons S '()) '()))))))
            (else
             (cond
               ((< S (first l))
                (cond
                  ((hasLeftSubtree l) (balanceTree(cons (first l) (cons (addNode S (firstChild l)) (cons (secondChild l) '())))))
                  (else (cons (first l) (cons (cons S '()) (cons (secondChild l) '()))))))
               (else
                (cond
                  ((hasRightSubtree l) (balanceTree (cons (first l) (cons (firstChild l) (cons (addNode S (secondChild l)) '())))))
                  (else (cons (first l) (cons (firstChild l) (cons (cons S '()) '())))))))))))

(define hasOnlyLeftSubtree
  (lambda (l)
    (and (hasLeftSubtree l) (not (hasRightSubtree l)))))

(define hasOnlyRightSubtree
  (lambda (l)
    (and (hasRightSubtree l) (not (hasLeftSubtree l)))))

(define hasBothSubtrees
  (lambda (l)
    (and (hasLeftSubtree l) (hasRightSubtree l))))

(define findLargestElm
  (lambda (l)
    (cond
      ((or (terminalNode? l) (hasOnlyLeftSubtree l)) (first l))
      ((hasRightSubtree l) (findLargestElm (secondChild l))))))

(define removeNode
  (lambda (S l)
    (cond
      ((= S (first l))
       (cond
         ((terminalNode? l) '())
         ((hasOnlyLeftSubtree l) (firstChild l))
         ((hasOnlyRightSubtree l)  (secondChild l))
         ((hasBothSubtrees l)
          (cons
           (findLargestElm (firstChild l))
           (cons (removeNode (findLargestElm (firstChild l)) (firstChild l))
                 (cons (secondChild l) '()))))))
      ((terminalNode? l) l) ;;;;
      ((< S (first l))
        (cons (first l)
              (cons (removeNode S (firstChild l))
                    (cons (secondChild l) '()))))
       ((> S (first l))
        (cons (first l)
              (cons (firstChild l)
                    (cons (removeNode S (secondChild l)) '())))))))

 ;(cond ((< S (first l)) (cons (balanceTree(addNode S (firstChild l))) (cons (secondChild l) '())))
  ;(else (cons (firstChild l) (cons (balanceTree (addNode S (secondChild l))) '())))))))))