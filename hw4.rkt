#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

(define (sequence low high stride)
  (cond [(> low high) null]
        [#t (cons low (sequence (+ low stride) high stride))]))

(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(empty? xs) (error "list-nth-mod: empty list")]
        [#t (car (list-tail xs (remainder n (length xs))))]))

(define (stream-for-n-steps s n)
  (letrec ([f (lambda (s n ans)
                (let ([pr (s)])
                  (if (= n 0) null (cons (car pr) (f (cdr pr) (- n 1) (+ ans 1))))))])
    (f s n 1)))

(define funny-number-stream
  (letrec ([f (lambda (x)
                (cond [(= (remainder x 5) 0) (cons (- x) (lambda () (f (+ x 1))))]
                      [#t (cons x (lambda () (f (+ x 1))))]))])
    (lambda () (f 1))))

(define stream-dan-then-dog
  (letrec ([f (lambda (x)
                (if (false? x) (cons "dog.jpg" (lambda () (f #t))) (cons "dan.jpg" (lambda () (f #f)))))])
    (lambda () (f #t))))
