#!r6rs

(import (rnrs base)
        (rnrs io simple)
         (a-d memory manual vectors exponential (3)))
  
;; UTILS
;;======

(define (make-block siz)
  (pop-free (locate-free siz)))
(define (free-block blk)
  (push-free blk))

(define (print-memory)
  (display (list->vector (debug-memory)))
  (newline))


;; TESTS
;;======

(define (test-1)
  (print-memory)
  (let [(v1 (make-block 10))
        (v2 (make-block 10))
        (v3 (make-block 10))]
    (print-memory)      ;should have 4 blocks of size 16 (3 allocated + 1 locally-free)
    (free-block v1)
    (print-memory)      ;should have 4 blocks of size 16 (2 allocated + 2 locally-free)
    (free-block v3)
    (print-memory)      ;should have 4 blocks of size 16 (1 allocated + 1 locally-free + 2 globally-free)
    (free-block v2)
    (print-memory)))    ;all memory should be merged again into a single block

;sketch for yourself what the memory should look like after every step
(define (test-2)
  (print-memory)
  (let [(v1 (make-block 20))
        (v2 (make-block 20))
        (v3 (make-block 20))]
    (print-memory)
    (free-block v1)
    (print-memory)
    (free-block v3)
    (print-memory)
    (let [(v4 (make-block 10))]
      (print-memory)
      (free-block v2)
      (free-block v4)
      (print-memory))))

(define (test-3)
  ;make as many vectors as you can until memory is full
  (define vs (let loop () 
               (let [(v (make-block 10))]
                 (if (eq? v null)
                     '()
                     (cons v (loop))))))
  ;deallocate 4 of them
  (free-block (list-ref vs 0))
  (free-block (list-ref vs 1))
  (free-block (list-ref vs 2))
  (free-block (list-ref vs 3))
  (print-memory)
  ;this will only work if all locally-free memory has been coalesced
  (let [(new-vec (make-block 40))] 
    (assert (not (eq? new-vec null)))
    (print-memory)))


;; USAGE
;;=======

;; e.g.
;; > (initialize)
;; > (test-1)