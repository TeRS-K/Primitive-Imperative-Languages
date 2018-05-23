#lang racket
;cooperation of Qingyun (Teresa) Kang (q7kang) and Peter Ralbovsky (pdralbov)
 
;memory direct access
(define memory (make-hash ))
(define (set-memory-value!  name value)
  (if (hash-has-key? memory name) (error "duplicate") (hash-set! memory name value)))
(define (memory-lookup name)
  (hash-ref memory name (lambda ()(begin (error "undefined")))))

;phases
;1. resolve halt and lit
;2. store all psymbols
;3. resolve const chains
;4. replace psymbols occurences with the respective psymbols and resolve


;phase 1
(define (resolve-halt-lit l)
  (if (empty? l) empty
      (cons
       (match (car l)
         [`(halt) 0]
         [`(lit ,x) x]
         [x x]
         )
       (resolve-halt-lit (rest l)))))

;phase 2
;new featrues:
;with non const psymbols I am adding information about how they were created data/label

(define (store-psymbols l i)
  (if (empty? l) empty
       (let ([translated-commands
              (match (car l)
                [`(const ,name ,value) (begin (set-memory-value! name value) empty)]
                [`(data ,name (,n ,value)) (begin (set-memory-value! name (list 'data i)) (make-list n value))]
                [`(data ,name ,value ...)  (begin (set-memory-value! name (list 'data i)) value)]
     
                [`(label ,name) (begin (set-memory-value! name (list 'label i)) empty )]
                [x (list x)]
                )])
         (append translated-commands (store-psymbols (rest l) (+ i (length translated-commands)))))))


;phase 3
;replacing all psymbols with their values
;note that new definition of resolve, now name must be name of const 

(define (resolve name)
  (define memval (memory-lookup name))
  (hash-set! memory name 'resolving)
  (define toreturn
    (match memval
      ['resolving (error "circular")]
      [(? symbol? x) (let ([resolved-x (resolve x)]) (begin (hash-set! memory name resolved-x) resolved-x))]
      [`(data ,x) x]
      [`(label ,x) x] ; this case should not happen
      [x x]
    ))
  (if (and (symbol? (memory-lookup name)) (symbol=? (memory-lookup name) 'resolving))
      (begin (hash-set! memory name memval) toreturn)
      toreturn
  ))

;(define (resolve name start)
;  (if (and (symbol? name) (symbol? start) (symbol=? name start)) (error "circular")
;      (match name
;    [`(,i) (list i)]
;    [(? number? x) x]
;    [x (begin
;         (defined actual-value)
;         (hash-set! memory name (resolve (memory-lookup name) (if (boolean? start) name start)))
;         (memory-lookup name))]
;  )))

(define (resolve-everything)
  (hash-for-each memory
                 (lambda (name value)(resolve name) )))

;phase 4
;replacing all psymbols and resolving them

(define (resolve-literal-or-constant x) (if (symbol? x) (memory-lookup x) x))

;(define (unbracket x)
;  (if (symbol? x) (first (resolve-literal-or-constant x)) x))

(define (resolve-dest-or-opd dest target)
  (lambda (x)
    (if (not (symbol? x))
        (match x
          [`(,a ,b) (list
                     (match (resolve-literal-or-constant a)
                       [`(data ,x) x]
                       [`(label ,x) (error "incorrect")]
                       [x x])
                     (match (resolve-literal-or-constant b)
                       [`(data ,x) (list x)]
                       [`(label ,x) (error "incorrect")]
                       [`(,x) (list x)]
                       [x (error "incorrect")]))]
          [a a])
        (match (memory-lookup x)
          [`(label ,x) (if target x (error "incorrect"))] 
          [`(data ,x) (list x)]
          [x (if dest (begin  (error "incorrect")) x)]
          ))))

(define (replace-literals-in-instruction i)
  (match i
    [`(branch ,opd ...)  (cons 'branch (map (resolve-dest-or-opd false true) opd))] 
    [`(jump ,opd)  (list 'jump ((resolve-dest-or-opd false true)opd)) ]
    [`(print-string ,x) (list 'print-string x)]
    [`(,fun ,opd) (list fun ((resolve-dest-or-opd false false) opd))]  ; print-val
    [`(,fun ,dest ,opd ...) (append (list fun ((resolve-dest-or-opd true false) dest))  ;
                                    (map (resolve-dest-or-opd false false) opd))]   
    [x (match (resolve-literal-or-constant x)
         [`(label ,x) x]
         [`(data ,x) x]
         [a a])]))

(define (replace-literals-in-list l)
  (map replace-literals-in-instruction l)
  )

(define (primp-assemble l)
  (set! memory (make-hash))
  
  (define prog1 (store-psymbols (resolve-halt-lit l) 0))
  ;(println memory)
  (resolve-everything)
  ;(println memory)
  (replace-literals-in-list prog1))

(define test3
'(
(label START)
(mul (MEH SIU) Y B)
(print-val (MEH SIU))
(print-string "\n")
(halt)
(const Y X)
(data MEH X #f START #t Y)
(data SIU (4 START))
(const X 13)
(data A 1)
(data B A)
(const F E)
(const E D)
(const D C)
(const C Z)
(const Z 4)
(jump START)
))

(define test4
  '(
    (label START)
    (label START2)
    (gt TMP1 N 1)
    (lnot TMP1 TMP1)
    (branch TMP1 LOOPCONT)
    (jump LOOPEND)
    (label LOOPCONT)
    (mul (23 RES) RES N)
    (lit RES)
    (lit 68)
    (sub (SEX (1)) N 1)
    (sub (SEX SEX) N (1))
    (jump START)
    (label LOOPEND)
    (print-val RES)
    ;(mul pi 0 (5 SEX))
    (print-val PHI)
    (print-val TMP1)
    (halt)
    (data N 10)
    (data MESS (4 69))
    (data SEX (5 RES))
    (data RES 1)
    (const pi 314)
    (const e pi)
    (const phi START)
    (data PHI phi)
    (data TMP1 0)
    (data A1 e)
    (label L)
    (data A2 A1)
    (data L1 L)
    (data A3 A4)
    (data A4 A3)
    (const C1 A3)
    ;(const D1 D2)
    ;(const D2 D1)
    ))
(primp-assemble
 test4
)