#lang racket/base

(require
 racket/contract racket/match
 yutes abstraction)

(provide
 (contract-out
  (struct zipper ([data list?] [context list?])))
 zipper-has-context?
 zipper-has-data?
 zipper-of?
 zipperof
 make-zipper
 build-zipper
 empty-zipper
 zipper-empty?
 zipper->list
 zipper->list/reverse
 zipper-equal?
 zipper-fwd
 zipper-bwd
 zipper-top
 zipper-push
 zipper-splice
 zipper-monad
 zipper-map)



(struct zipper
  (data context)
  #:prefab)

(define/match/wow-contract (zipper-has-context? xs)
  (-> zipper? boolean?)
  [((zipper _ '())) #f]
  [(_) #t])


(define/match/wow-contract (zipper-has-data? xs)
  (-> zipper? boolean?)
  [((zipper '() _)) #f]
  [(_) #t])


(define/wow-contract (zipper-of? pred? xs)
  (-> predicate/c zipper? boolean?)
  (and (zipper? xs)
    (for/fold ([result #t])
	([x (zipper->list xs)] #:break (not result))
      (pred? x))))

(define-syntax zipperof
  (syntax-rules ()
    [(_ pred?)
     (rename-contract
      (lambda (xs) (zipper-of? pred? xs))
      `(zipperof 'pred?))]))






(define/wow-contract (make-zipper . xs)
  (->* () #:rest (listof any/c) (and/c zipper? (not/c zipper-has-context?)))
  (zipper xs '()))

(define/wow-contract (build-zipper n proc)
  (-> natural-number/c (-> natural-number/c any/c) zipper?)
  (zipper (build-list n proc) '()))


(define/contract empty-zipper
  zipper?
  (zipper '() '()))

(define/match/wow-contract (zipper-empty? xs)
  (-> zipper? boolean?)
  [((zipper '() '())) #t]
  [(_) #f])



(define/match/wow-contract (zipper->list xs)
  (-> zipper? list?)
  [((zipper data context))
   (rappend context data)])



(define/match/wow-contract (zipper->list/reverse xs)
  (-> zipper? list?)
  [((zipper data context))
   (rappend context data)])


(define/wow-contract (zipper-equal? xs ys)
  (-> zipper? zipper? boolean?)
  (equal? (zipper->list xs)
	  (zipper->list ys)))


(define/match/wow-contract (zipper-fwd xs)
  (-> (and/c zipper? zipper-has-data?) zipper?)
  [((zipper (cons x data) context))
   (zipper data (cons x context))])


(define/match/wow-contract (zipper-bwd xs)
  (-> (and/c zipper? zipper-has-context?) zipper?)
  [((zipper data (cons x context))) (zipper (cons data) context)])


(define/wow-contract (zipper-front xs)
  (-> zipper? (and/c zipper? (not/c zipper-has-context?)))
  (zipper (zipper->list xs) '()))


(define/wow-contract (zipper-back xs)
  (-> zipper? (and/c zipper? (not/c zipper-has-data?)))
  (zipper '() (zipper->list/reverse xs)))


(define/match/wow-contract (zipper-position xs)
  (-> zipper? natural-number/c)
  [((zipper _ context)) (length context)])


(define/match/wow-contract (zipper-remaining xs)
  (-> zipper? natural-number/c)
  [((zipper data _)) (length data)])



(define (zipper-length xs)
  (-> zipper? natural-number/c)
  (+ (zipper-remaining xs)
     (zipper-position xs)))


(define/match/wow-contract (zipper-move-by xs n)
  (->i ([xs zipper?]
	[n (xs) (lambda (n)
		  (cond [(= n 0) #t]
			[(> n 0) (<= n (zipper-remaining xs))]
			[(< n 0) (>= n (zipper-position xs))]))])
       () [result zipper?])
  [(xs 0) xs]
  [(xs n) #:when (> n 0) (zipper-move-by (zipper-fwd xs) (sub1 n))]
  [(xs n) #:when (< n 0) (zipper-move-by (zipper-bwd xs) (add1 n))])


(define/wow-contract (zipper-move-to xs p)
  (->i ([xs zipper?]
	[p (xs) (and/c natural-number/c
		       (lambda (p)
			 (<= p (zipper-length xs))))])
       () [result (xs p) (and zipper?
			   (lambda (result)
			     (and (= (zipper-position result) p)
			       (zipper-equal? xs result))))])
  (zipper-move-by xs (- p (zipper-position xs))))


(define/match/wow-contract (zipper-top xs)
  (-> (and/c zipper? zipper-has-data?) any/c)
  [((zipper (cons x _) _)) x])

(define/match/wow-contract (zipper-push xs x)
  (-> zipper? any/c zipper?)
  [((zipper data context) x)
   (zipper (cons x data) context)])


(define/match/wow-contract (zipper-splice xs ys)
  (->i ([xs zipper?]
	[ys zipper?])
       () [result (xs ys)
		  (lambda (result)
		    (= (zipper-length result)
		       (+ (zipper-length xs)
			  (zipper-length ys))))])
  [((zipper x-data x-context) (zipper y-data y-context))
   (zipper (append x-data y-data) (append x-context y-context))])


(define/match/wow-contract (zipper-trans f xs)
  (-> (-> any/c any/c) zipper? zipper?)
  [(f (zipper data context))
   (zipper (map f data) (map f context))])

(define/wow-contract (zipper-return x)
  (-> any/c zipper?)
  (zipper (list x) '()))

(define/match/wow-contract (zipper-join xss)
  (-> zipper? zipper?)
  [((zipper data context))
   (zipper (apply append (map zipper->list data))
	   (apply append (map zipper->list/reverse context)))])

(define/wow-contract (zipper-app fs xs)
  (-> (zipperof (-> any/c any/c)) zipper? zipper?)
  (zipper-join (zipper-trans (lambda (f) (zipper-trans f xs)) fs)))




(define/match/wow-contract (zipper-zapp fs xs)
  (-> (zipperof (-> any/c any/c)) zipper? zipper?)
  [((zipper fdata fcontext) (zipper xdata xcontext))
   (zipper (map call fdata xdata)
	   (map call fcontext xcontext))])



(define/match/wow-contract (zipper-bind xs f)
  (-> zipper? (-> any/c zipper?) zipper?)
  [((zipper '() '()) f) empty-zipper]
  [((zipper (cons x xs) '()) f)
   (zipper-splice (zipper-front (f x)) (zipper-bind (zipper xs '()) f))]
  [((zipper '() (cons x xs)) f)
   (zipper-splice (zipper-back (f x)) (zipper-bind (zipper '() xs) f))]
  [((zipper data context) f)
   (zipper-splice (zipper-bind (zipper data '()) f)
		  (zipper-bind (zipper '() context) f))])


(define/match/wow-contract (zipper-extract xs)
  (-> (and/c zipper? zipper-has-data?) any/c)
  [((zipper (cons x xs) _)) x])


(define/match/wow-contract (zipper-duplicate xs)
  (-> zipper? (zipperof zipper?))
  [((zipper data context))
   (zipper (for/list ([i (in-range (length data))])
	     (zipper-move-by xs i))
	   (for/list ([i (in-range (length context))])
	     (zipper-move-by xs -i)))])

(define/match/wow-contract (zipper-extend xs f)
  (-> (and/c zipper? zipper-has-data?) (-> zipper? any/c) any/c)
  [(xs f) (zipper-trans f (zipper-duplicate xs))])


(define/wow-contract (zipper-map f xs . xss)
  (->i ([f procedure?]
	[xs zipper?])
       #:rest [xss (f) (and/c (listof zipper?)
			      (lambda (xss) (procedure-arity-includes?
					 f (add1 (length xss)))))]
       [result zipper?])
  (zipper-move-to
   (zipper (apply map f (zipper->list xs) (map zipper->list xss)) '())
   (zipper-position xs)))



(define zipper-monad
  (monad
   zipper-trans
   zipper-app
   zipper-return
   zipper-bind
   zipper-join))

(module+ test
  (require rackunit)
  (define (sqr x) (* x x))
  (define (twc x) (+ x x))
  (let ([xs (make-zipper 1 2 3)])
    (check-true (zipper? xs))    
    (check-equal? (zipper-length xs) 3)
    (check-equal? (zipper-position xs) 0)
    (check-equal? (zipper-remaining xs) (zipper-length xs))
    (check-true (zipper-has-data? xs))
    (check-false (zipper-has-context? xs))
    (check-equal? xs (zipper '(1 2 3) '()))
    (begin
      (for/fold ([xs* xs])
	  ([i (in-range (zipper-length xs))])	
	(check-equal? (zipper-position xs*) i)
	(check-equal? xs* (zipper-move-by xs i))
	(zipper-fwd xs*))
      (void))

    (check-equal?
     (call-with zipper-monad
       (let-m ([x xs])
	 (return (sqr x))))

     (zipper (map sqr (zipper->list xs))
	     '()))

    (check-equal? 
     (zipper-map twc xs) 
     (zipper-map + xs xs))


    (check-equal?
     (call-with zipper-monad
       (let-m ([x xs])
	 (return (sqr x))))
     (zipper-map sqr xs)))) ; end of submodule test



