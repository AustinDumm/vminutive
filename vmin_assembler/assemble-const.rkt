#lang racket

(require "assembler-utilities.rkt")

(provide process-const-commands)

(define (is-const-def command)
  (and (list? command)
       (eq? 'DEF-CONST (first command))))

(define (process-const name size)
  (let ((const-value (hash-ref const-lookup
                               name)))
    (generate-literal const-value size)))

(define const-use-handlers
  (hash
   'CONST8 (lambda (name)
             (process-const name 1))
   'CONST16 (lambda (name)
              (process-const name 2))
   'CONST32 (lambda (name)
              (process-const name 4))
   'CONST64 (lambda (name)
              (process-const name 8))))

(define (fetch-const-value command)
  (let* ((const-command (list-ref command 0))
         (const-name (list-ref command 1))
         (const-handler (hash-ref const-use-handlers
                                  const-command)))
    (const-handler const-name)))

(define (is-const-use command)
  (and (list? command)
       (hash-has-key? const-use-handlers
                      (first command))))

(define const-lookup
  (make-hash))

(define (process-const-defs instruction-list)
  (let* ((pulled-defs (tear is-const-def
                            instruction-list))
         (defs (list-ref pulled-defs 0))
         (instructions (list-ref pulled-defs 1)))
    (for ([def defs])
      (let ((name (list-ref def 1))
            (value (list-ref def 2)))
        (hash-set! const-lookup
                   name
                   value)))
    instructions))

(define (process-const-use instruction-list)
  (map (lambda (instruction)
         (if (is-const-use instruction)
             (fetch-const-value instruction)
             instruction))
       instruction-list))

(define (process-const-commands
         instruction-list)
  (let* ((processed-instrs (process-const-defs
                            instruction-list))
         (final-instrs (process-const-use
                        processed-instrs)))
    final-instrs))