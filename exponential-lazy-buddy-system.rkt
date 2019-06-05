#lang r6rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*              Exponential Buddy System Memory Manager            *-*-
;-*-*                  (optimised freelist management)                *-*-
;-*-*                 Theo D'Hondt - Wolfgang De Meuter               *-*-
;-*-*               1993-2009 Programming Technology Lab              *-*-
;-*-*                   Vrije Universiteit Brussel                    *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(library
 (exponential)
 (export initialize size-free overhead null locate-free pop-free push-free peek
         poke! debug-memory memory free-tab free-tail-tab size-tab d-tab)
 (import (rnrs base)
         (rnrs control)
         (rnrs mutable-pairs)
         (only (rnrs io simple) display newline))
 
 (define overhead 1)
 (define null '())
 
 (define memory-size (expt 2 6))
 (define memory  (make-vector memory-size null))
 
 (define (peek addr)
   (vector-ref memory addr))
 
 (define (poke! addr value)
   (vector-set! memory addr value))
 
 (define lower-size 16)
 
 (define max-index (let loop
                     ((size memory-size)
                      (index 0))
                     (if (< size lower-size)
                         index
                         (loop (div size 2) (+ index 1)))))

 (define make-d cons)
 
 (define free-tab (make-vector max-index))
 (define free-tail-tab (make-vector max-index))
 (define size-tab (make-vector max-index))
 (define d-tab (make-vector max-index))
 
 (define (size index)
   (vector-ref size-tab (- index 1)))
 
 (define (size! index size)
   (vector-set! size-tab (- index 1) size))
 
 (define (free index)
   (vector-ref free-tab (- index 1)))
 
 (define (free! index free)
   (vector-set! free-tab (- index 1) free))
 
 (define (free-tail index)
   (vector-ref free-tail-tab (- index 1)))
 
 (define (free-tail! index free)
   (vector-set! free-tail-tab (- index 1) free))

 ;Hou voor elke index een paar bij om de D-index te kunnen berekenen.
 ;in de car het aantal gealloceerde blokken,
 ;in de cdr het aantal lokaal vrije blokken
 (define (d-tab! index d-pair)
   (vector-set! d-tab (- index 1) d-pair))

 ;bereken de D-index door het aantal lokaal vrije blokken
 ;af trekken van het aantal gealloceerde blokken
 (define (d index)
   (let ((d-idx (vector-ref d-tab (- index 1))))
     (- (car d-idx) (cdr d-idx))))


 ;hulpfunctie om aantal allocaties te incrementeren
 (define (allocate-inc index)
   (let ((d-i (vector-ref d-tab (- index 1))))
     ;(display d-i)(newline)
     (set-car! d-i (+ 1 (car d-i)))
     (vector-set! d-tab (- index 1) d-i)))
 
 ;hulpfunctie om aantal allocaties te decrementeren
 (define (allocate-dec index)
   (let ((d-i (vector-ref d-tab (- index 1))))
     (set-car! d-i (- (car d-i) 1))
     (vector-set! d-tab (- index 1) d-i)))
 
 ;hulpfunctie om aantal vrije locatie te incrementeren
 (define (loc-free-inc index)
   (let ((d-i (vector-ref d-tab (- index 1))))
     (set-cdr! d-i (+ 1 (cdr d-i)))
     (vector-set! d-tab (- index 1) d-i)))

 ;hulpfunctie om aantal vrije locaties te decrementeren
 (define (loc-free-dec index)
   (let ((d-i (vector-ref d-tab (- index 1))))
     (set-cdr! d-i (- (cdr d-i) 1))
     (vector-set! d-tab (- index 1) d-i)))
 
 (define (index addr)
   (peek (+ addr 0)))
 
 (define (index! addr index)
   (poke! (+ addr 0) index))
 
 (define (next addr)
   (peek (+ addr 1)))
 
 (define (next! addr next)
   (poke! (+ addr 1) next))
 
 (define (previous addr)
   (peek (+ addr 2)))
 
 (define (previous! addr next)
   (poke! (+ addr 2) next))
 
 (define (locate-free req-size)
   (define (locate first last)
     (if (<= first last)
         (let 
             ((here (div (+ first last) 2)))
           (if (> req-size (size here))
               (locate (+ here 1) last)
               (locate first (- here 1))))
         first))
   (define index (locate 1 max-index))
   (if (> index max-index)
       null
       index))
 
 (define (negative? n) (< n 0))
 (define (abs n) (if (negative? n) (- n) n))


 ; doorloop het geheugen van begin tot eind
 ; en beschrijf elk geheugenblok door adres, type en grootte terug te geven in een lijst
 (define (debug-memory)
   (define f-tab (vector-map (lambda (x) x) free-tab))

   (let loop-mem
     ((addr 0)
      (result ()))
     (let*
         ((idx (abs (peek addr)))
          (size (size (abs idx)))
          (free? (and (number? (free idx)) (= (free idx) addr)))
          (status (cond ((not free?) 'allocated)
                        ((negative? (peek addr)) 'globally-free)
                        (else 'locally-free)))
          (has-next? (> (- memory-size 1) (+ addr size))))
       
       (if (and has-next? free?)
           (vector-set! f-tab (- idx 1) (next addr)))

       (if has-next?
           (loop-mem (+ addr size) (cons (list addr status size) result))
           (reverse (cons (list addr status size) result))
           )))) 
 
 (define (size-free addr)
   (size (index addr)))

 (define (split index)
   (if (= index max-index)
       null
       (let
           ((addr (pop-free (+ index 1))))
         (if (null? addr)
             null
             (let*
                 ((size (size index))
                  (buddy-addr (+ addr size)))
               (next!     buddy-addr null)
               (previous! buddy-addr null)
               (free!  index buddy-addr)
               (allocate-inc index)
               (loc-free-inc index)
               (free-tail!  index buddy-addr)
               (index! buddy-addr index)
               (index! addr index)
               addr)))))

 ; alloceer een geheugenblok
 (define (pop-free index)
   (define addr-local (free index))
   (define addr-global (free-tail index))
   ;(display (list 'pop-free index addr-local addr-global))(newline)
   (cond
     ((not (or (null? addr-local) (negative? (peek addr-local))))     ; als een blok lokaal vrij is (index is niet negatief),
      (pop-free-local index addr-local))                              ; alloceer dit blok
     ((and (not (null? addr-global)) (negative? (peek addr-global)))  ; als een blok globaal vrij is, alloceer dit blok 
      (pop-free-global index addr-global))
     (else (split index))))                                           ; anders recursief splitsen tot er blok van de gevraagde
 ; grootte is gevonden

 
 (define (pop-free-local index addr)
   (allocate-inc index)                 ; incrementeer allocatie op de index 
   (loc-free-dec index)                 ; decrementeer lokaal vrije index
   (pop-freelist! index)                ; verwijder eerste element van de freelist
   addr)
    
    
 (define (pop-free-global index addr)
   (allocate-inc index)                 ; incrementeer allocatie op de index 
   (extract-freelist! index)            ; verwijder laatste element van de freelist
   addr)
   
          

 ; voeg toe aan het eind van de freelist
 (define (push-freelist-tail! addr index)
   (let 
       ((prev (free-tail index)))
     (index! addr (- index))
     (next!     addr null)
     (previous! addr prev)
     (if (not (null? prev))
         (next! prev addr)
         (free! index addr))   ; als de freelist ervoor leeg was, pas het begin van freelist nu ook aan
     (index! addr (- index))   ; markeer als globaal vrij door de index negatief te maken
     (free-tail!  index addr)
     )
   )
 
 ; verwijder laatste element van de freelist
 (define (extract-freelist! idx)
   (define addr (free-tail idx))
   (if (null? addr)
       addr
       (let
           ((prev (previous addr)))
         
         (free-tail! idx prev)       ; het voorlaatste element wordt het laatste
         
         (if (not (null? prev))
             (next! prev null)
             (free! idx prev))       ; als de freelist ervoor leeg was, pas het begin van freelist nu ook aan
         (index! addr idx)
         addr))
   )

 ; voeg element toe aan begin van de freelist
 (define (push-freelist-head! addr index)
   (let 
       ((next (free index)))
     (next!     addr next)
     (previous! addr null)
     (if (not (null? next))
         (previous! next addr)
         (free-tail! index addr))   ; als de freelist ervoor leeg was, pas het einde van freelist nu ook aan
     (index! addr  index)
     (free!  index addr)
     (loc-free-inc index)
     (allocate-dec index)
     )
   )

 ; verwijder het eerste element van de freelist
 (define (pop-freelist! idx)
   (define addr (free idx))
   (if (null? addr)
       addr
       (let 
           ((next (next addr)))
         (free! idx next)
         (if (not (null? next))
             (previous! next null)
             (free-tail! idx null))   ; als de freelist ervoor leeg was, pas het einde van freelist nu ook aan
         (index! addr idx)
         addr))
   )

 ; dealloceer een geheugenblok
 ; haal de D-index op van de index en volg de regels
 ; - als Di >= 2 maak dit blok lokaal vrij
 ; - als Di = 1 maak dit blok globaal vrij
 ; - als Di = 0 maak dit blok lokaal vrij en maak een willekeurig blok op deze index vrij
 (define (push-free addr)
   ;(if (and (number? addr) (positive? addr) (< addr (- memory-size 1)))
   (let*
       ((idx (abs(index addr)))
        (d (d idx)))
     (cond
       ((> d 1) (push-free-locally addr idx))
       ((> d 0) (push-free-globally addr idx)
                (allocate-dec idx))                   ; dit blok was voorheen gealloceerd, dus dealloceer
       ((= 0 d) (push-free-globally addr idx)
                (allocate-dec idx)                    ; dit blok was voorheen gealloceerd, dus dealloceer
                (let ((free-addr (free idx)))         ; maak het eerstvolgende element in de freelist ook globaal vrij
                  (pop-freelist! idx)                 ; dit blok was voorheen lokaal vrij, dus pop uit de freelist
                  (push-free-globally free-addr idx) 
                  (loc-free-dec idx))))))             ; dit blok was voorheen lokaal vrij, dus decrement de lokaal vrije teller

 ; maak een blok lokaal vrij
 (define (push-free-locally addr index)
   (push-freelist-head! addr index))     ; het enige dat moet gebeuren, is het blok vooraan de freelist toevoegen
   
 
 ; Maak een blok globaal vrij.
 ; Als het globaal vrij te maken blok een buddy heeft die ook globaal vrij is, merge deze recursief.
 ; Als dit niet kan, voeg het blok toe aan het eind van de freelist en markeer het als globaal vrij.
 (define (push-free-globally addr idx)
   (define (merge addr addr-index)
     (if (< addr-index max-index)
         (let*
             ((block-size (size addr-index))
              (left-buddy (even? (div addr block-size)))
              (buddy-addr ((if left-buddy + -) addr block-size))
              (buddy-index (index buddy-addr)))
           (if (= addr-index (- buddy-index))      ; test dat de buddy index dezelfde is, en dat de buddy globaal vrij is
               (let 
                   ((next (next buddy-addr))
                    (previous (previous buddy-addr)))
                 (if (null? previous)
                     (free! addr-index next)
                     (next! previous   next))
                 (if (not (null? next))
                     (previous! next previous)
                     (free-tail! addr-index null)) ; als de freelist ervoor leeg was, pas het einde van freelist nu ook aan
                    
                 (index! (if left-buddy addr buddy-addr) (+ addr-index 1)) ; nu de beide buddies zijn gemerged, probeer dit te doen voor de volgende index (recursie)
                 (push-free  (if left-buddy addr buddy-addr))              ; Hier is het onmogelijk te weten of het blok op de volgende index lokaal dan wel
                 #t)                                                       ; globaal moet worden vrijgemaakt, vandaar de sprong naar `push-free` waar die beslissing
               #f))                                                        ; wel kan worden gemaakt
         #f))
   
   (define (push addr index)
     (if (not (merge addr index))
         (push-freelist-tail! addr index))) ; als er niet gemerged kan worden, voeg het blok toe aan het einde van de freelist

   (push addr idx))
 
 (define (initialize)
   (let loop 
     ((index 1)
      (size lower-size))
     (when (<= index max-index)
       (size! index size)
       (free! index null)
       (free-tail! index null)
       (d-tab! index (make-d 0 0))
       (loop (+ index 1) (* size 2))))
   (poke! 0 (- (locate-free memory-size)))
   (free!  max-index 0)
   (free-tail!  max-index 0)
   ))