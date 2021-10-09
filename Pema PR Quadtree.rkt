
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

(define terminalNode?
  (lambda (S)
    (= 2 (length S))))

(define first
  (lambda (p)
    (car p)))

(define second
  (lambda (p)
    (car (cdr p))))

(define third
  (lambda (p)
    (car (cdr (cdr p)))))

(define fourth
  (lambda (p)
    (car (cdr (cdr (cdr p))))))

(define getX
  (lambda (S)
    (car S)))

(define getY
  (lambda (S)
    (car (cdr S))))

(define areEqual?
  (lambda (A B)
    (and (= (getX A) (getX B)) (= (getY A) (getY B)))))

(define whichQuadrant
  (lambda (x y w x1 y1)
    (cond
      ((and (<= (- x1 x) (/ w 2)) (<= (- y1 y) (/ w 2))) 'NW)
      ((and (<= (- x1 x) (/ w 2)) (>= (- y1 y) (/ w 2))) 'SW)
      ((and (>= (- x1 x) (/ w 2)) (<= (- y1 y) (/ w 2))) 'NE)
      ((and (>= (- x1 x) (/ w 2)) (>= (- y1 y) (/ w 2))) 'SE))))

(define searchInProperQuadrant
  (lambda (S l x y w Quad)
    (cond
       ((eq? 'NW Quad) (findNode S (first l) x y (/ w 2)))
       ((eq? 'SW Quad) (findNode S (second l) x (+ y (/ w 2)) (/ w 2)))
       ((eq? 'NE Quad) (findNode S (third l) (+ x (/ w 2)) y (/ w 2)))
       ((eq? 'SE Quad) (findNode S (fourth l) (+ x (/ w 2))  (+ y (/ w 2)) (/ w 2))))))

(define addInProperQuadrant
  (lambda (S l x y w Quad)
    (cond
       ((eq? 'NW Quad) (cons (insertNode S (first l) x y (/ w 2)) (cons (second l) (cons (third l) (cons (fourth l) '())))))
       ((eq? 'SW Quad) (cons (first l) (cons (insertNode S (second l) x (+ y (/ w 2)) (/ w 2)) (cons (third l) (cons (fourth l) '())))))
       ((eq? 'NE Quad) (cons (first l) (cons (second l) (cons (insertNode S (third l) (+ x (/ w 2)) y (/ w 2)) (cons (fourth l) '())))))
       ((eq? 'SE Quad) (cons (first l) (cons (second l) (cons (third l) (cons (insertNode S (fourth l) (+ x (/ w 2))  (+ y (/ w 2)) (/ w 2)) '()))))))))

(define removeInProperQuadrant
  (lambda (S l x y w Quad)
    (cond
       ((eq? 'NW Quad) (cons (removeNode S (first l) x y (/ w 2)) (cons (second l) (cons (third l) (cons (fourth l) '())))))
       ((eq? 'SW Quad) (cons (first l) (cons (removeNode S (second l) x (+ y (/ w 2)) (/ w 2)) (cons (third l) (cons (fourth l) '())))))
       ((eq? 'NE Quad) (cons (first l) (cons (second l) (cons (removeNode S (third l) (+ x (/ w 2)) y (/ w 2)) (cons (fourth l) '())))))
       ((eq? 'SE Quad) (cons (first l) (cons (second l) (cons (third l) (cons (removeNode S (fourth l) (+ x (/ w 2))  (+ y (/ w 2)) (/ w 2)) '()))))))))

(define findNode
  (lambda (S l x y w)
      (cond
        ((null? l) '())
        ((terminalNode? l)
         (cond
           ((areEqual? S l) l)
           (else '())))
        (else (searchInProperQuadrant S l x y w (whichQuadrant x y w (getX S) (getY S)))))))

(define insertNode
  (lambda (S l x y w)
    (cond
      ((null? l) S)
      ((terminalNode? l) (insertNode S (insertNode l '(() () () ()) x y w) x y w))
      (else (addInProperQuadrant S l x y w (whichQuadrant x y w (getX S) (getY S)))))))

(define shouldTheyBeMerged
  (lambda (l)
    (cond ((terminalNode? l) 0)
          (else
           (cond
             ((and
               (null? (first l))
               (null? (second l))
               (null? (third l))
               (null? (fourth l)))
              1)
             ((and
               (not (null? (first l)))
               (null? (second l))
               (null? (third l))
               (null? (fourth l)))
              2)
             ((and
               (null? (first l))
               (not (null? (second l)))
               (null? (third l))
               (null? (fourth l)))
              3)
             ((and
               (null? (first l))
               (null? (second l))
               (not (null? (third l)))
               (null? (fourth l)))
              4)
             ((and
               (null? (first l))
               (null? (second l))
               (null? (third l))
               (not (null? (fourth l))))
              5)
             (else 0))))))
(define mergeNodes
  (lambda (l)
    (cond
      ((= 0 (shouldTheyBeMerged l)) l)
      ((= 1 (shouldTheyBeMerged l)) '())
      ((= 2 (shouldTheyBeMerged l)) (first l))
      ((= 3 (shouldTheyBeMerged l)) (second l))
      ((= 4 (shouldTheyBeMerged l)) (third l))
      ((= 5 (shouldTheyBeMerged l)) (fourth l)))))

(define removeNode
  (lambda (S l x y w)
    (cond
      ((null? l) '())
      ((terminalNode? l)
       (cond
         ((areEqual? S l) '())
         (else l)))
      (else (mergeNodes (removeInProperQuadrant S l x y w (whichQuadrant x y w (getX S) (getY S))))))))