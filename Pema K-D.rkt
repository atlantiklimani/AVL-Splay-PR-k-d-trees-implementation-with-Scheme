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

(define terminalNode?
  (lambda (l)
    (cond ((= 2 (length l)) #t)
          (else #f))))

(define firstChild
  (lambda (l)
   (cond ((terminalNode? l) '()) (else(car (cdr l))))))

(define secondChild
  (lambda (l)
    (cond ((terminalNode? l) '()) (else (car (cdr (cdr l)))))))

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

(define getX
  (lambda (S)
    (car S)))

(define getY
  (lambda (S)
    (car (cdr S))))

(define xLevel?
  (lambda (d)
    (cond ((= 0 (modulo d 2)) #t)
          (else #f))))

(define yLevel?
  (lambda (d)
    (cond ((= 1 (modulo d 2)) #t)
          (else #f))))

(define areEqual?
  (lambda (A B)
    (and (= (getX A) (getX B)) (= (getY A) (getY B)))))

(define findNode
  (lambda (S l d)
    (cond
      ((null? l) '())
      ((terminalNode? l)
       (cond ((areEqual? S l) l)
             (else '())))
      ((areEqual? S (first l)) (first l)) ;;;;
      (else
       (cond ((xLevel? d)
              (cond ((< (getX S) (getX (first l))) (findNode S (firstChild l) (+ 1 d)))
                    (else (findNode S (secondChild l) (+ 1 d)))))
             (else
              (cond ((< (getY S) (getY (first l))) (findNode S (firstChild l) (+ 1 d)))
                    (else (findNode S (secondChild l) (+ 1 d))))))))))

(define insertNode
  (lambda (S l d)
    (cond
      ((null? l) '())
      ((terminalNode? l)
       (cond ((xLevel? d)
              (cond ((< (getX S) (getX l)) (cons l (cons S (cons '() '()))))
                    (else (cons l (cons '() (cons S '()))))))
             (else
              (cond ((< (getY S) (getY l)) (cons l (cons S (cons '() '()))))
                    (else (cons  l (cons '() (cons S '()))))))))
      (else
       (cond ((xLevel? d)
              (cond ((< (getX S) (getX (first l))) (cons (first l) (cons (insertNode S (firstChild l) (+ 1 d)) (cons (secondChild l) '()))))
                    (else (cons (first l) (cons (firstChild l) (cons (insertNode S (secondChild l) (+ 1 d)) '()))))))
             (else
              (cond ((< (getY S) (getY (first l))) (cons (first l) (cons (insertNode S (firstChild l) (+ 1 d)) (cons (secondChild l) '()))))
                    (else (cons (first l) (cons (firstChild l) (cons (insertNode S (secondChild l) (+ 1 d)) '())))))))))))

;(define returnLargestOf
;  (lambda (A B)
;    (cond ((> A B) A)
;          (else B))))

(define returnLargestOf
  (lambda (A B c)
    (cond
      ((xLevel? c)
       (cond ((> (getX A) (getX B)) A)
             (else B)))
       (else
        (cond ((> (getY A) (getY B)) A)
              (else B))))))

(define returnSmallestOf
  (lambda (A B c)
    (cond
      ((xLevel? c)
       (cond ((< (getX A) (getX B)) A)
             (else B)))
       (else
        (cond ((< (getY A) (getY B)) A)
              (else B))))))

;(define findLargestElm 
;  (lambda (l d c)   ; niveli fillestar ku fillon kerkimi nga nje funksion tjeter
;    (cond
;      ((null? l) '0)
;      ((terminalNode? l)
;       (cond
;         ((xLevel? c) (first l))
;         (else second l)))
;     ((xLevel? c)
;       (cond ((xLevel? d)
;              (cond ((hasRightSubtree l) (findLargestElm (secondChild l) (+ 1 d) c))
;                    (else (first l))))
;             (else  (returnLargestOf (getX (first l)) (returnLargestOf (findLargestElm (firstChild l) (+ 1 d) c) (findLargestElm (secondChild l) (+ 1 d) c))))))
;      (else                     ;;else i bjen qe eshte yLevel
;       (cond ((yLevel? d)
;              (cond ((hasRightSubtree l) (findLargestElm (secondChild l) (+ 1 d) c))
;                    (else (first l))))
;              (else (returnLargestOf (getY (first l)) (returnLargestOf (findLargestElm (firstChild l) (+ 1 d) c) (findLargestElm (secondChild l) (+ 1 d) c)))))))))

(define findLargestElm 
  (lambda (l d c)   ; niveli fillestar ku fillon kerkimi nga nje funksion tjeter
    (cond
      ((null? l) '(0 0))
      ((terminalNode? l) l)
      ((xLevel? c)
       (cond ((xLevel? d)
              (cond ((hasRightSubtree l) (findLargestElm (secondChild l) (+ 1 d) c))
                    (else (first l))))
             (else  (returnLargestOf (first l) (returnLargestOf (findLargestElm (firstChild l) (+ 1 d) c) (findLargestElm (secondChild l) (+ 1 d) c) c) c))))
      (else                     ;;else i bjen qe eshte yLevel
       (cond ((yLevel? d)
              (cond ((hasRightSubtree l) (findLargestElm (secondChild l) (+ 1 d) c))
                    (else (first l))))
              (else (returnLargestOf (first l) (returnLargestOf (findLargestElm (firstChild l) (+ 1 d) c) (findLargestElm (secondChild l) (+ 1 d) c) c) c)))))))

(define findSmallestElm 
  (lambda (l d c)   ; niveli fillestar ku fillon kerkimi nga nje funksion tjeter
    (cond
      ((null? l) '(0 0))
      ((terminalNode? l) l)
      ((xLevel? c)
       (cond ((xLevel? d)
              (cond ((hasLeftSubtree l) (findSmallestElm (firstChild l) (+ 1 d) c))
                    (else (first l))))
             (else  (returnSmallestOf (first l) (returnSmallestOf (findSmallestElm (firstChild l) (+ 1 d) c) (findSmallestElm (secondChild l) (+ 1 d) c) c) c))))
      (else                     ;;else i bjen qe eshte yLevel
       (cond ((yLevel? d)
              (cond ((hasLeftSubtree l) (findSmallestElm (firstChild l) (+ 1 d) c))
                    (else (first l))))
              (else (returnSmallestOf (first l) (returnSmallestOf (findSmallestElm (firstChild l) (+ 1 d) c) (findSmallestElm (secondChild l) (+ 1 d) c) c) c)))))))

(define removeNode
  (lambda (S l d)
    (cond
           ((null? l) '())
           ((terminalNode? l)
            (cond
              ((areEqual? S l) '())
              (else l)))
           ((areEqual? S (first l))
            (cond
             ((terminalNode? l) '())
             ((hasOnlyLeftSubtree l) (cons (findLargestElm (firstChild l) d d) (removeNode ((findLargestElm (firstChild l) d d) (firstChild l) (+ 1 d)))))
             ((hasOnlyRightSubtree l) (cons (findSmallestElm (secondChild l) d d) (removeNode (findSmallestElm (secondChild l) d d) (secondChild l) (+ 1 d))))
             ((hasBothSubtrees l) (cons (findLargestElm (firstChild l) d d) (cons (removeNode (findLargestElm (firstChild l) d d) (firstChild l) (+ 1 d)) (cons (secondChild l) '())))))) 
           ((xLevel? d)
            (cond
             ((< (getX S) (getX (first l)))
              (cons (first l) (cons (removeNode S (firstChild l) (+ 1 d)) (cons (secondChild l) '()))))
             (else (cons (first l) (cons (firstChild l) (cons (removeNode S (secondChild l) (+ 1 d)) '()))))))
           ((yLevel? d)
            (cond
             ((< (getY S) (getY (first l)))
              (cons (first l) (cons (removeNode S (firstChild l) (+ 1 d)) (cons (secondChild l) '()))))
             (else (cons (first l) (cons (firstChild l) (cons (removeNode S (secondChild l) (+ 1 d)) '())))))))))

