#lang racket

(provide process-label-commands)

(require racket/match)
(require "assembler-utilities.rkt")

(define label-def-lookup
  (make-hash))
(define (handle-def-label . args)
  (let ((name (list-ref args 0))
        (line (list-ref args 1)))
    (hash-set! label-def-lookup
               name
               line)))

(define (handle-label32 . args)
  (let* ((name (list-ref args 0))
         (line (hash-ref label-def-lookup name)))
    (generate-literal line 4)))

(define (handle-label64 . args)
  (let* ((name (list-ref args 0))
         (line (hash-ref label-def-lookup name)))
    (generate-literal line 8)))

(define def-label-commands
  `((DEF-LABEL 0 ,handle-def-label)))

(define def-label-command-handlers
  (apply hash
         (flatten
          (map (lambda (def) (list (list-ref def 0)
                                   (list-ref def 2)))
               def-label-commands))))

(define use-label-commands
  `((LABEL32 4 ,handle-label32)
    (LABEL64 8 ,handle-label64)))

(define (use-command? command)
  (and (list? command)
       (hash-has-key? use-label-command-handlers
                      (first command))))

(define (handle-use-command command)
  ((hash-ref use-label-command-handlers
             (first command))
   (list-ref command 1)))

(define (def-label-command? command)
  (and (list? command)
       (hash-has-key?
            def-label-command-handlers
            (first command))))

(define use-label-command-handlers
  (apply hash
         (flatten
          (map (lambda (def) (list (list-ref def 0)
                                   (list-ref def 2)))
               use-label-commands))))
(define use-label-command-width
  (apply hash
         (flatten
          (map (lambda (def) (list (list-ref def 0)
                                   (list-ref def 1)))
               use-label-commands))))

(define (zip . lists)
  (apply map list lists))

(define (command-widths
         instructions)
  (map (lambda (instruction)
         (match instruction
           [`(,command ,_)
            #:when (hash-has-key? use-label-command-width
                                  command)
            (hash-ref use-label-command-width
                      command)]
           [`(,command ,_)
            #:when (hash-has-key? def-label-command-handlers
                                  command)
            0]
           [l #:when (list? l)
              (length (flatten l))]
           [_ 1]))
       instructions))

(define (scan proc list initial)
  (define (helper proc list last-result running)
    (if (null? list)
        (reverse running)
        (let* ((next (first list))
               (next-result (proc last-result next)))
          (helper proc
                  (rest list)
                  next-result
                  (cons next-result running)))))
  (helper proc list initial '()))

(define (drop-last n list)
  (reverse (drop (reverse list) n)))

(define (pair-command-addrs instructions)
  (let* ((widths (command-widths instructions))
         (shifted-widths (cons 0 widths))
         (addrs (scan + shifted-widths 0))
         (addrs (drop-last 1 addrs)))
    (zip instructions addrs)))

(define (def-label-addr-pair? pair)
  (def-label-command? (first pair)))

(define (process-label-commands
         instruction-list)
  (let* ((command-addrs (pair-command-addrs
                         instruction-list))
         (defs-commands (tear def-label-addr-pair?
                              command-addrs))
         (defs (list-ref defs-commands 0))
         (command-addrs (list-ref defs-commands 1)))
    (for ([def defs])
      (let* ((def-command (list-ref def 0))
             (def-label (list-ref def-command 0))
             (def-name (list-ref def-command 1))
             (def-line (list-ref def 1))
             (def-handler (hash-ref
                           def-label-command-handlers
                           def-label)))
        (def-handler def-name def-line)))
    (map (lambda (command-addr)
           (let ((command (list-ref command-addr 0)))
             (if (use-command? command)
                 (handle-use-command command)
                 command)))
         command-addrs)))
        
          