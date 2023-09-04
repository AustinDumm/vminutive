#lang racket

(require "instructions.rkt")
(require "assemble-literals.rkt")
(require "assemble-labels.rkt")
(require "assemble-const.rkt")
(require "assembler-utilities.rkt")

(define (build-vmin-string
         string)
  (define byte-list (bytes->list (string->bytes/utf-8 string)))
  (define len (length byte-list))

  (cons len byte-list))

(define (process-chars-commands
         instruction-list)
  (append-map (lambda (instruction)
                (if (string? instruction)
                    (build-vmin-string instruction)
                    (list instruction)))
              instruction-list))

(define (place-instr-end
         instruction-list)
  (append instruction-list '((DEF-LABEL instr_end))))

(define (place-const-mem-limit
         instruction-list)
  (append instruction-list `((DEF-CONST mem_limit ,(expt 2 32)))))

(define (is-lib-command command)
  (and (list? command)
       (eq? 'LIB (first command))))

(define lib-prefix-list (list))
(define (process-lib-prefixes
         instruction-list)
  (append-map (lambda (instruction)
                (if (is-lib-command instruction)
                    (begin
                      (set! lib-prefix-list (append lib-prefix-list (list (list-ref instruction 1))))
                      '())
                    (list instruction)))
              instruction-list))
  

(define included-set (mutable-set))
(define (fetch-include instruction)
  (define file-name (list-ref instruction 1))

  (if (set-member? included-set file-name)
      '()
      (begin
        (set-add! included-set file-name)
        (grab-include-file file-name))))

(define (grab-include-file file-name)
  (define (try-open-file file-name prefixes)
    (if (empty? prefixes)
        (open-input-file file-name)
        (with-handlers ([exn:fail:filesystem:errno?
                         (lambda (_) 
                           (try-open-file file-name (rest prefixes)))])
          (let* ((next-prefix (first prefixes))
                 (env-prefix (getenv next-prefix))
                 (next-prefix (if (boolean? env-prefix) next-prefix env-prefix))
                 (full-file-name (string-append next-prefix "/" file-name))
                 (input-file (open-input-file full-file-name)))
            input-file))))

  (define file (try-open-file file-name lib-prefix-list))
  (define file-data (read file))
  (close-input-port file)
  (process-include file-data))

(define (include? instruction)
  (and (list? instruction)
       (eq? 'INCLUDE (first instruction))))

(define (process-include
         instruction-list)
  (process-lib-prefixes (append-map (lambda (instruction)
                (if (include? instruction)
                    (fetch-include instruction)
                    (list instruction)))
              instruction-list)))

(define (script? instruction)
  (and (list? instruction)
       (eq? 'SCRIPT (first instruction))))

(define-namespace-anchor anc)
(define namespace (namespace-anchor->namespace anc))
(define (process-scripts
         instruction-list)
  (append-map (lambda (instruction)
         (if (script? instruction)
             (eval (list-ref instruction 1) namespace)
             (list instruction)))
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
                 "Invalid item found in instructions"
                 instruction))))
       instruction-list))

(define assemble
  (compose
   (lambda (instructions)
     (instructions->codes instructions
                          instruction-lookup))
   flatten
   process-label-commands
   process-chars-commands
   process-const-commands
   place-const-mem-limit
   process-lit-commands
   process-scripts
   process-include
   process-lib-prefixes))

(display (assemble (read)))
