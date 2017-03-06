;; store.scm

;; =============== Store ====================

;; the-store! is the store!
(define the-store! 'uninitialized)
(define current-p 0)
(define init-size 20)

;; (empty-store) return an empty Scheme list representing the empty
;; store.
(define empty-store
	(lambda ()
		(make-vector init-size)))

;; (initialize-store!) it initializes the-store! to (empty-store)
(define initialize-store!
	(lambda ()
		(set! the-store! (empty-store))))

;; (newref! val) 
;; if current pointer is smaller to size(there is space to add a new ref), 
;; takes a value val adds to the the-store!, and returns a ref-val to the added value val.
;; else, grow the size of store, add the val and return ref-val
(define newref!
	(lambda (val)
		(cond
			[ (< current-p (vector-length the-store!))
				(vector-set! the-store! current-p val)
				(set! current-p (+ current-p 1))
				(ref-val (- current-p 1))]
			[else
				(set! the-store! (vector-grow the-store! (make-vector (* (+ current-p 1) 2)) 0))
				(vector-set! the-store! current-p val)
				(set! current-p (+ current-p 1))
				(ref-val (- current-p 1))
		]
	)))

;;copy every val of vec to new-vec, return new-vec
;;note: the length of new-vec must >= length of vec
(define vector-grow
	(lambda (vec new-vec curr)
		(cond
			[(< curr (vector-length vec))
				(vector-set! new-vec curr (vector-ref vec curr))  ;copy the val at index curr
				(vector-grow vec new-vec (+ curr 1))]  ;recur on the next index
			[else
					new-vec
			 ]
		)))
	

;; (deref ev) expects that ev is a reference (ref-val ref), and
;; returns the value of associated with ref in the store.
(define deref
	(lambda (ev)
		(let 
			([ref (expval->ref ev)])
			(vector-ref the-store! ref))))

;; (setref! ev val) expects that ev is a reference (ref-val ref), and
;; it sets the reference ref to val in the the-store!
(define setref!
	(lambda (ev val)
		(let
			([ref (expval->ref ev)])
			(vector-set! the-store! ref val))))
