#lang racket
;cooperation of Qingyun (Teresa) Kang (q7kang) and Peter Ralbovsky (pdralbov)
(provide compile-simp)
(define (var-append x)
  (string->symbol (string-append "_" (symbol->string x))))

(define loop-num 0)

(define (app-in x num) (string->symbol (string-append  (symbol->string x) (number->string num))))
(define trans-op (hash '* 'mul '+ 'add '- 'sub 'div 'div 'mod 'mod
                           '= 'equal '> 'gt '< 'lt '>= 'ge '<= 'le 'and 'land 'or 'lor 'not 'lnot))
(define (parse-bin op x y)
  `(,@(parse-inst x)
    ,@(parse-inst y)
    (,(hash-ref trans-op op) (-2 SP) (-2 SP) (-1 SP))
    (sub SP SP 1)
    )
  )
(define (parse-inst l)
  (match l
    [`(print ,x) (if (string? x)
                     (list (list 'print-string x))
                     `(,@(parse-inst x)
                       (print-val (-1 SP))
                       (sub SP SP 1)
                           )
                     )]
     [`(set ,x ,y) `(,@(parse-inst y)
                     (move ,(var-append x) (-1 SP))
                     (sub SP SP 1)
                     )]
    [`(seq ,x ...) (append* (map parse-inst x))]
    [`(iif ,x ,y ,z)
     
     (set! loop-num (+ 1 loop-num))
     (let ([num loop-num])
       `(,@(parse-inst x)
         (sub SP SP 1)
       (branch (0 SP) ,(app-in 'iftrue num))
       (jump ,(app-in 'iffalse num))
       (label ,(app-in 'iftrue num))          
       ,@(parse-inst y)
       (jump ,(app-in 'endif num))
       (label ,(app-in 'iffalse num))
       ,@(parse-inst z)
       (label ,(app-in 'endif num))))
     
       ]
    [`(skip) empty]
    [`(vars ,l ,r ...) (append (append* (map parse-inst r))
                               (list (list 'halt))
                               (map (lambda (x)
                                      `(data ,(var-append (first x))
                                             ,(second x)))
                                    l))]
    [`(while ,b ,stmt ...)
     (set! loop-num (+ 1 loop-num))
     (let ([num loop-num])
     `((label ,(app-in 'loop-cond num))
       ,@(parse-inst b)
       (sub SP SP 1)
       (branch (0 SP) ,(app-in 'loop-begin num))
       (jump ,(app-in 'loop-end num))
       (label ,(app-in 'loop-begin num))
       ,@(append* (map parse-inst stmt))
       (jump ,(app-in 'loop-cond num))
       (label ,(app-in 'loop-end num))
       ))]
    [`(not ,x) `(,@(parse-inst x) (lnot (-1 SP) (-1 SP)))]
    [`(,op ,x ,y) (parse-bin op x y)]
    [`(or ,l ...) (if (= 1 (length l)) (first l)
                  (parse-bin 'or (first l) (cons 'or (rest l))))]
    [`(and ,l ...) (if (= 1 (length l)) (first l)
                  (parse-bin 'and (first l) (cons 'and (rest l))))]
    [(? number? x)
     `((move (0 SP) ,x)
       (add SP SP 1))]
    ['true `((move (0 SP) #t)
       (add SP SP 1))]
    ['false `((move (0 SP) #f)
       (add SP SP 1))]
    [x `((move (0 SP) ,(var-append x))
       (add SP SP 1))]
    [x x]
    
    
    ))
(define (compile-simp x)
  `(,@(parse-inst x)
    (data SP stack)
    (data stack 0)))

(define test1
  '((fun (fact-h n acc)
  (vars [(a 1) (b 2)]
    (iif (> n 0)
         (return acc)
         (return
          (fact-h
           (-n 1)
           (* n acc))))))

(fun (main) 
	(vars [(x 0)]
		(set x (fact-h 4 0))
		(return x)))))


(define test2
  '(vars [(i 1) (j 0) (acc 0)] 
         (while (<= i 10000) 
                (set j 1) 
                (set acc 0) 
                (while (< j i)
                       (iif (= (mod i j) 0) 
                            (set acc (+ acc j)) 
                            (skip)) 
                       (set j (+ j 1))) 
                (iif (= acc i) 
                     (seq 
                      (print i) 
                      (print "\n")) 
                     (skip)) 
                (set i (+ i 1)))))

(compile-simp test1)
