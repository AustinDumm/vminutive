(module assembler-instructions racket
  
  (provide instruction-lookup)

  (define instructions
    '(push
      push-stack-count
      push-addr-count
      push-addr
      pop
      pop-addr
      duplicate
      duplicate-addr
      swap
      swap-add
      pop-to-stack
      pop-to-addr
      duplicate-from-addr
      duplicate-to-addr
      add-i
      add-f
      subtract-i
      subtract-f
      multiply-i
      multiply-f
      divide-i
      divide-f
      modulus
      shift-right
      shift-left
      rotate-right
      rotate-left
      bitwise-and
      bitwise-or
      bitwise-xor
      bitwise-not
      jump
      jump-eqz-i
      jump-eqz-f
      jump-ltz-i
      jump-ltz-f
      jump-gtz-i
      jump-gtz-u
      jump-gtz-f
      jump-lez-i
      jump-lez-f
      jump-gez-i
      jump-gez-f
      jump-nez-i
      jump-nez-f
      jump-inf-f
      jump-nan-f
      call
      store1
      store2
      store3
      store4
      store5
      store6
      store7
      store8
      load1
      load2
      load3
      load4
      load5
      load6
      load7
      load8
      port-register
      is-port-registered
      port-wait-register
      port-send
      port-read
      port-disconnect
      push-port
      die
      spawn
      send
      expect
      observe))

  (define (enumerate list)
    (zip list (range (length list))))

  (define (zip first second)
    (map cons first second))

  (define instruction-tuples
    (map (lambda (pair)
           (list (car pair) (cdr pair)))
         (enumerate instructions)))

  (define instruction-lookup
    (apply hash (flatten instruction-tuples))))