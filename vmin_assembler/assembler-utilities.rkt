#lang racket

(provide generate-literal
         tear)

(define (generate-literal literal size)
  (define full-byte 256)

  (define (raise-size-error literal size)
    (raise-user-error
             (~a "Literal provided does not fit in requested byte size." #\newline
                 "Literal: " literal #\newline
                 "Requested Size: " size #\newline
                 "Required Size: >=" (+ (quotient literal full-byte) 1))))

  (define (helper number bytes running)
    (if (= bytes 0)
        (if (= number 0)
            running
            (raise-size-error literal size))
        (helper (quotient number full-byte)
                (- bytes 1)
                (cons (remainder number full-byte)
                      running))))

  (define (literal->unsigned literal)
    (if (< literal 0)
        (+ 1 (bitwise-xor (- (expt 2 (* size 8)) 1) (abs literal)))
        literal))
  
  (reverse (helper (literal->unsigned literal) size '())))

(define (tear predicate list)
  (let* ((torn (foldl (lambda (item acc)
                        (let ((in (list-ref acc 0))
                              (out (list-ref acc 1)))
                          (if (predicate item)
                              `(,(cons item in) ,out)
                              `(,in ,(cons item out)))))
                      '(() ())
                      list))
         (first (list-ref torn 0))
         (second (list-ref torn 1)))
    `(,(reverse first) ,(reverse second))))