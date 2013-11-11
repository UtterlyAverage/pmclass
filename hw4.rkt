#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; 1
(define (sequence low high stride)
  (cond [(> low high) null]
        [#t (cons low (sequence (+ low stride) high stride))]))

;; 2
(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

;; 3
(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(empty? xs) (error "list-nth-mod: empty list")]
        [#t (car (list-tail xs (remainder n (length xs))))]))

;; 4
(define (stream-for-n-steps s n)
  (letrec ([f (lambda (s n ans)
                (let ([pr (s)])
                  (if (= n 0) null (cons (car pr) (f (cdr pr) (- n 1) (+ ans 1))))))])
    (f s n 1)))

;; 5
(define funny-number-stream
  (letrec ([f (lambda (x)
                (cond [(= (remainder x 5) 0) (cons (- x) (lambda () (f (+ x 1))))]
                      [#t (cons x (lambda () (f (+ x 1))))]))])
    (lambda () (f 1))))

;; 6
(define dan-then-dog
  (letrec ([f (lambda (x)
                (if (false? x) (cons "dog.jpg" (lambda () (f #t))) (cons "dan.jpg" (lambda () (f #f)))))])
    (lambda () (f #t))))

;; 7
;; Switched from letrec to define as lecture notes say it is prefered style.
(define (stream-add-zero s)
  (define (f s) 
    (cons (cons 0 (car (s))) (lambda () (f (cdr (s))))))
  (lambda () (f s)))

;; 8
(define (cycle-lists xs ys)
  (define (f xs xy n)
    (cons (cons (list-nth-mod xs n) 
                (list-nth-mod ys n)) 
          (lambda () (f xs ys (+ 1 n)))))
  (lambda () (f xs ys 0)))

;; 9
(define (vector-assoc v vec)
  (define (f v vec n)
    (if (= (vector-length vec) n) #f 
        (let ([elem (vector-ref vec n)])
          (if (pair? elem)
              (if (equal? (car elem) v) elem (f v vec (+ 1 n)))
              (f v vec (+ 1 n))))))
  (f v vec 0))

;; 10
(define (cached-assoc xs n)
  (define cache (make-vector n #f))
  (define slot 0)
  (define (check-cache key)
    (if (= (vector-assoc cache key) #f) #f key))
  (define (add-to-cache key)
    (define assoc-return (assoc xs key))
    (if (= assoc-return #f) #f 
        (begin
          (vector-set! cache (remainder n slot) assoc-return)
          (set! slot (+ 1 slot))
          assoc-return)))
  (lambda (v) 
    (define cache-return (vector-assoc cache v))
    (if (not (= cache-return #f)) cache-return (add-to-cache v))))
