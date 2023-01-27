#lang racket

(require racket/format)
(require "instructions.rkt")
(require "assembler-utilities.rkt")

(provide process-lit-commands)

(define lit-commands
  `((LIT8 1 ,(lambda (args)
              (generate-literal
               (list-ref args 0)
               1)))
    (LIT16 2 ,(lambda (args)
            (generate-literal
             (list-ref args 0)
             2)))
    (LIT32 4 ,(lambda (args)
            (generate-literal
             (list-ref args 0)
             4)))
    (LIT64 8 ,(lambda (args)
            (generate-literal
             (list-ref args 0)
             8)))))
     

(define (lit-command? command)
  (and (list? command)
       (hash-has-key? lit-command-handler-lookup
                      (first command))))
(define lit-command-handler-lookup
  (apply hash
         (flatten (map (lambda (command-def)
                         (list (list-ref command-def 0)
                               (list-ref command-def 2)))
                       lit-commands))))

(define (unpack-lit-command
         command)
  (let* ((command-name (first command))
         (command-args (rest command))
         (command-handler (hash-ref
                           lit-command-handler-lookup
                           command-name)))
    (command-handler command-args)))

(define (process-lit-commands
         instruction-list)
   (map (lambda (instruction)
         (if (lit-command? instruction)
             (unpack-lit-command instruction)
             instruction))
        instruction-list))