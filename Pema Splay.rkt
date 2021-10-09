#lang scheme

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define lat?
  (lambda (l)
    (cond ((null? l) #t)
          ((atom? (car l))(lat? (cdr l)))
          (else #f))))

;(define member?
 ; (lambda (a lat)
  ;  (cond ((null? lat) #f)
   ;       ((eq?  a (car lat)) #t)
    ;      (else (member? a (cdr lat))))))

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

(define firstChild
  (lambda (l)
   (cond
     ((terminalNode? l) '())
     (else(car (cdr l))))))

(define secondChild
  (lambda (l)
    (cond ((terminalNode? l) '()) (else (car (cdr (cdr l)))))))

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

(define hasLeftSubtree             ; is there a left subtree
  (lambda (l)
    (cond ((null? (car (cdr l))) #f)
          (else #t))))

(define hasRightSubtree            ; is there a right subtree
  (lambda (l)
    (cond ((null? (car (cdr (cdr l)))) #f)
          (else #t))))

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

(define rightRotation
  (lambda (l)
    (cons (first (firstChild l)) (cons (firstChild(firstChild l)) (cons (cons (first l) (cons (secondChild (firstChild l)) (cons (secondChild l) '()))) '())))))
   

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

(define right-rightRotation
  (lambda (l)
    (rightRotation (rightRotation l))))

(define left-leftRotation
  (lambda (l)
    (leftRotation (leftRotation l))))

;(define splayTree
 ; (lambda (l)
  ;  (cond
    ;  ((terminalNode? l) l)
   ;   ((= 1 (- (height (firstChild l)) (height (secondChild l)))) (rightRotation l))
     ; ((= -1 (- (height (firstChild l)) (height (secondChild l)))) (leftRotation l))
      ;((= 2 (- (height (firstChild l)) (height (secondChild l))))
      ; (cond
       ;  ((= -1 (- ()))))))))

;(define member*
; (lambda (S l)
;   (cond ((= (first l) S) #t)
;         ((terminalNode? l) #f)
;         ((= (first (firstChild l)) S) #t)
;         ((= (first (secondChild l)) S) #t)
;         (else #f))))

(define member*
  (lambda (S l)
    (cond ((null? l) #f)
          ((= (first l) S) #t)
          ((terminalNode? l) #f)
          (else (or (member* S (firstChild l)) (member* S (secondChild l)))))))

(define returnNode
  (lambda (s l)
    (cond ((null? l) '())
          ((= s (first l)) l)                                   
          (else (cond  ((terminalNode? l) '())
                 (else
                 (cond
                   ((= s (first l)) l)
                        (else
                              (cond
                                ((< s (first l))
                                 (splayTree s
                                            (cons (first l)
                                                  (cons
                                                   (returnNode s (firstChild l))
                                                   (cons (secondChild l) '())))))

                                (else (splayTree s
                                                 (cons (first l)
                                                       (cons (firstChild l)
                                                             (cons
                                                              (returnNode s (secondChild l)) '()))))))))))))))

(define countDepthOf    ; perdore kur je i sigurt qe nyja S permbahet ne pemen l
  (lambda (S l)
    (cond
      ((= (first l) S) 0)
      (else
       (cond
         ((< S (first l)) (+ 1 (countDepthOf S (firstChild l)))) 
         ((> S (first l)) (+ 1 (countDepthOf S (secondChild l)))))))))

(define splayTree
  (lambda (S l)
    (cond ((null? l) '())
          ((= S (first l)) l)
          ((terminalNode? l) '())
          ((member* S (firstChild l))
           (cond
             ((= 1 (countDepthOf S (firstChild l) ))
                  (cond
                    ((not (hasLeftSubtree (firstChild l))) (left-rightRotation l)) ;;;;;;;;;
                        ((= S (first (firstChild (firstChild l)))) (right-rightRotation l))
                        (else (left-rightRotation l))))
                 (else l)))
          ((member* S (secondChild l))
           (cond ((= 1 (countDepthOf S (secondChild l)))
                  (cond
                    ((not (hasRightSubtree (secondChild l))) (right-leftRotation l)) ;;;;;;
                        ((= S (first (secondChild (secondChild l)))) (left-leftRotation l))
                        (else (right-leftRotation l))))
                 (else l)))
          (else '()))))


(define splay
  (lambda (S l)
    (cond ((null? l) '())
          ((and (not (null? (firstChild l))) (= S (first (firstChild l)))) (rightRotation l))
          ((and (not (null? (secondChild l))) (= S (first (secondChild l)))) (leftRotation l))
          (else l))))

(define findNode
  (lambda (S l)
    (splay S (returnNode S l))))

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
                  ((hasLeftSubtree l)
                   (splayTree S
                              (cons
                               (first l)
                               (cons (addNode S (firstChild l)) (cons (secondChild l) '())))))
                  (else (cons (first l) (cons (cons S '()) (cons (secondChild l) '()))))))
               (else
                (cond
                  ((hasRightSubtree l)
                   (splayTree S
                              (cons
                               (first l)
                               (cons (firstChild l) (cons (addNode S (secondChild l)) '())))))
                  (else (cons (first l) (cons (firstChild l) (cons (cons S '()) '())))))))))))

(define insertNode
  (lambda (S l)
    (splay S (addNode S l))))

(define removeNode
  (lambda (S l P)
    (cond
      ((= S (first l))
       (cond
         ((terminalNode? l) '())
         ((hasOnlyLeftSubtree l) (firstChild l))
         ((hasOnlyRightSubtree l)  (secondChild l))
         ((hasBothSubtrees l)
           (cond ((= (findLargestElm (firstChild l)) (first (firstChild l))) (cons (first (firstChild l)) (cons '() (cons (secondChild l) '()))))
                 (else (cons
                         (findLargestElm (firstChild l))
                         (cons (splay (findParentOf (findLargestElm (firstChild l)) (firstChild l)) (removeNode (findLargestElm (firstChild l)) (firstChild l) (findParentOf (findLargestElm (firstChild l)) (firstChild l)) ))
                               (cons (secondChild l) '()))))))))
       ((< S (first l))
        (splayTree P (cons (first l)
              (cons (removeNode S (firstChild l) P)
                    (cons (secondChild l) '())))))
       ((> S (first l))
        (splayTree P (cons (first l)
              (cons (firstChild l)
                    (cons (removeNode S (secondChild l) P) '()))))))))

(define findParentOf ;; metoda perdoret vetem pasi jemi siguruar qe nyja S permbahet ne pemen l dhe qe pema ka se paku nje dege
  (lambda (S l)
    (cond ((< S (first l))
           (cond ((= S (first (firstChild l))) (first l))
                 (else (findParentOf S (firstChild l)))))
          (else
           (cond ((= S (first (secondChild l))) (first l))
                 (else (findParentOf S (secondChild l))))))))

(define deleteNode
  (lambda (S l)
    (cond ((and (member* S l) (not (= S (first l))))
           (splay (findParentOf S l) (removeNode S l (findParentOf S l))))
          (else l))))