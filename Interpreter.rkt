#lang racket
(require test-engine/racket-tests)
(struct bin (op fst snd) #:transparent)

(struct fun (param body) #:transparent)

(struct app (fn arg) #:transparent)

(struct seq (fast snd) #:transparent)

(struct set (var newval) #:transparent)

(struct result (val newstore) #:transparent)

(struct closure (var body envt) #:transparent)

(struct sub (name loc) #:transparent)
;; env is a list of subs
(struct st (loc val) #:transparent)
;; store is a list of st
(define (parse sx)
  (match sx
    [`(with ((,nm ,nmd)) ,bdy) (app (fun nm (parse bdy)) (parse nmd))]
    [`(+ ,x ,y) (bin '+ (parse x) (parse y))]
    [`(* ,x ,y) (bin '* (parse x) (parse y))]
    [`(- ,x ,y) (bin '- (parse x) (parse y))]
    [`(/ ,x ,y) (bin '/ (parse x) (parse y))]
    [`(fun (,x) ,bdy) (fun x (parse bdy))]
    [`(,f ,x) (app (parse f) (parse x))]
    [`(set ,nm ,x) (set (parse nm) (parse x))]
    [`(seq ,x ,y) (seq (parse x) (parse y))]
    [x x]))

(define (op-trans op)
  (match op
    ['+ +]
    ['* *]
    ['- -]
    ['/ /]))

(define (interp ast env store)
  (match ast
    [(fun v bdy)
     (result (closure v bdy env) store)]
    [(bin op x y)
     (let*
         ([result1 (interp x env store)]
          [newstore (result-newstore result1)]
          [result2 (interp y env newstore)])
       (result ((op-trans op)
                (result-val result1)
                (result-val result2))
               (result-newstore result2)))]
    [(seq x y)
     (interp y env (result-newstore (interp x env store)))]
    [(app fun-exp arg-exp)
     (match (interp fun-exp env store)
       [(result (closure fp fb fe) ns1)
        (let*
            ([result1 (interp arg-exp env ns1)]
             [val1 (result-val result1)]
             [ns2 (result-newstore result1)]
             [nl (length store)]
             [ne (cons (sub fp nl) env)]
             [ns3 (cons (st nl val1) ns2)])
          (interp fb ne ns3))])]
    [(set x y)
     (let*
         ([lx (lookuploc x env)]
          [result1 (interp y env store)]
          [ns (cons (st lx (result-val result1)) (result-newstore result1))])
       (result void ns))]
    [x (if (number? x)
           (result x store)         
           (result (lookupval (lookuploc x env) store) store))]))

(define (lookuploc name env)
  (cond
    [(empty? env) (error 'interp "unbound variable ~a" name)]
    [(symbol=? name (sub-name (first env))) (sub-loc (first env))]
    [else (lookuploc name (rest env))]))
    
(define (lookupval loc store)
  (cond
    [(empty? store) (error "value not found")]
    [(eq? loc (st-loc (first store))) (st-val (first store))]
    [else (lookupval loc (rest store))]))
