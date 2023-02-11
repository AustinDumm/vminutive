#lang racket

(require "instructions.rkt")
(require "assemble-literals.rkt")
(require "assemble-labels.rkt")
(require "assemble-const.rkt")
(require "assembler-utilities.rkt")

(define (place-instr-end
         instruction-list)
  (append instruction-list '((DEF-LABEL instr_end))))

(define (place-const-mem-limit
         instruction-list)
  (append instruction-list `((DEF-CONST mem_limit ,(expt 2 32)))))

(define (fetch-include instruction)
  (define file-name (list-ref instruction 1))
  (define file (open-input-file file-name))
  (define file-data (read file))
  (close-input-port file)
  (process-include file-data))

(define (include? instruction)
  (and (list? instruction)
       (eq? 'INCLUDE (first instruction))))

(define (process-include
         instruction-list)
  (append-map (lambda (instruction)
                (if (include? instruction)
                    (fetch-include instruction)
                    (list instruction)))
              instruction-list))

(define (script? instruction)
  (and (list? instruction)
       (eq? 'SCRIPT (first instruction))))

(define-namespace-anchor anc)
(define namespace (namespace-anchor->namespace anc))
(define (process-scripts
         instruction-list)
  (map (lambda (instruction)
         (if (script? instruction)
             (eval (list-ref instruction 1) namespace)
             instruction))
       instruction-list))

(define (instructions->codes
         instruction-list
         instruction-lookup)
  (map (lambda (instruction)
         (cond ((symbol? instruction)
                (hash-ref
                 instruction-lookup
                 instruction))
               ((number? instruction)
                instruction)
               (else
                (raise-user-error
                 "Invalid item found in instructions"))))
       instruction-list))

(define assemble
  (compose
   (lambda (instructions)
     (instructions->codes instructions
                          instruction-lookup))
   flatten
   process-label-commands
   process-const-commands
   place-const-mem-limit
   process-lit-commands
   process-scripts
   process-include))

(display (assemble (read)))