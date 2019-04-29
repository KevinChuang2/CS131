#lang racket
(define lambda_symbol (string->symbol "\u03BB"))
(define lambda_list (list lambda_symbol 'lambda))
(define (member? x list)
     (if (null? list) #f                               
         (if (equal? x (car list)) #t                   
              (member? x (cdr list)))))

(define (create_bindings x y x_dict y_dict)
  (cond
     [(and (equal? x '()) (equal? y '())) (list x_dict y_dict)]
     [(not (equal? (car x) (car y)))
       (create_bindings (cdr x) (cdr y) (append x_dict (list (list (car x) (excl-symbol (car x) (car y))))) (append y_dict (list (list (car y) (excl-symbol (car x) (car y))))))
       ]
      
      [else (create_bindings (cdr x) (cdr y ) x_dict y_dict)]
          )
  )
(define (check_dict x x_dict)
  (cond
    [(equal? x_dict '()) #f]
    [(member x (car x_dict)) #t]
    [else (check_dict x (cdr x_dict))]
   )
  )
(define (get_item x x_dict)
  (cond
    [(member x (car x_dict)) (car(cdr (car x_dict)))]
    [else (get_item x (cdr x_dict))]
   )
  )
(define (replace_with_bindings x y dict x_acc y_acc)
  (cond
    [(equal? x '()) (list x_acc y_acc)]
    [(list? (car x))
     (list 
     (append x_acc (list (car (replace_with_bindings (car x) (car y) dict '() '()))) (car (replace_with_bindings (cdr x) (cdr y) dict '() '() )))
     (append y_acc (list (car (cdr (replace_with_bindings (car x) (car y) dict '() '())))) (car (cdr (replace_with_bindings (cdr x) (cdr y) dict '() '() ))))
     )
     ]
    [(and (check_dict (car x) (car dict)) (check_dict (car y) (car (cdr dict))))
     (replace_with_bindings (cdr x) (cdr y) dict (append x_acc (list (get_item (car x) (car dict))))  (append y_acc (list (get_item (car y) (car (cdr dict))))))
     ]
    [(and (check_dict (car x) (car dict)) (not (check_dict (car y) (car (cdr dict)))))
     (replace_with_bindings (cdr x) (cdr y) dict (append x_acc (list (get_item (car x) (car dict))))  (append y_acc (list (car y))))
     ]
    [(and (not (check_dict (car x) (car dict))) (check_dict (car y) (car (cdr dict))))
     (replace_with_bindings (cdr x) (cdr y) dict (append x_acc (list (car x)))  (append y_acc (list (get_item (car y) (car (cdr dict))))))
     ]
    [else (replace_with_bindings (cdr x) (cdr y) dict (append x_acc (list (car x))) (append y_acc (list (car y))))]
    )
                       )
; returns a new symbol x!y
(define (excl-symbol x y)
	(string->symbol (string-append (symbol->string x) "!" (symbol->string y)))
)
(define (expr-compare x y)
(cond 
;equal case
[(equal? x y) x ]
;boolean case
[(and (boolean? x) (boolean? y))
(if x '% '(not %))]
;one is a list one isnt, terminate
[(not (and (list? x) (list? y)))
		 (list 'if '% x y)]
;list lengths aren't equal, terminate
[(not (equal? (length x) (length y)))
 (list 'if '% x y)]
;special case for quote, terminate
[(or (equal? (car x) 'quote) (equal? (car y) 'quote))
 (list 'if '% x y)
 ]
;only one if (y), terminate
[(and (not (equal? (car x) 'if)) (equal? (car y) 'if))
		 (list 'if '% x y)]
;only one if (x), terminate
[(and (not (equal? (car y) 'if)) (equal? (car x) 'if))
		 (list 'if '% x y)]



;both are some lambda, but lengths of arguments are diff
[(and (member? (car x) lambda_list) (member? (car y) lambda_list) (list? (car (cdr x))) (list? (car (cdr y))) (not (equal? (length(car (cdr x))) (length(car (cdr y))))))
 (list 'if '% x y)
  ]


;one is lamdba one is unicode, replace with unicode
[(and (not (equal? (car x) lambda_symbol)) (equal? (car y) lambda_symbol))
 (expr-compare (cons lambda_symbol (cdr x)) (cons lambda_symbol (cdr y))) 
 ]
;one is lamdba one is unicode, replace with unicode
[(and (equal? (car x) lambda_symbol) (not(equal? (car y) lambda_symbol)))
 (expr-compare (cons lambda_symbol (cdr x)) (cons lambda_symbol (cdr y))) 
 ]

;if both lambdas, but arguments are different and single arg
[(and (member? (car x) lambda_list) (member? (car y) lambda_list) (not (and (list? (car (cdr x))) (list? (car (cdr y))) )) (not (equal? (car (cdr x)) (car (cdr y)))))
 (expr-compare (car (replace_with_bindings x y (create_bindings (list (car(cdr x))) (list(car(cdr y))) '() '()) '() '())) (car (cdr(replace_with_bindings x y (create_bindings (list (car(cdr x))) (list(car(cdr y))) '() '()) '() '()))))
 ]
;if both lambdas, but arguments are different
[(and (member? (car x) lambda_list) (member? (car y) lambda_list) (not (equal? (car (cdr x)) (car (cdr y)))))
 (expr-compare (car (replace_with_bindings x y (create_bindings (car(cdr x)) (car(cdr y)) '() '()) '() '())) (car (cdr(replace_with_bindings x y (create_bindings (car(cdr x)) (car(cdr y)) '() '()) '() '()))))
 ]
;concatenate comparing the heads of two lists and compare the rest of the lists
[else (cons (expr-compare (car x) (car y)) (expr-compare (cdr x) (cdr y)))]
)
)
;confused because it never says to do anything about the binded variables? any test-expr-x with lambda will fail if binded variables
(define (test-expr-compare x y)
  (and (equal? (eval x) ((eval (list 'lambda '((% #t)) (expr-compare x y)))))
       (equal? (eval y) ((eval (list 'lambda '((% #f)) (expr-compare x y)))))))
;; see corresponding lines in `test-y` for how the x and y sides differ
(define test-expr-x
  '(list 
    (quote (a b c))
    '(d e f)
    (cons 'a '(b c d))
    (cons 'a '(b c d))
    (if (equal? '(a b) '(a b)) 'a 'b)
    (if (equal? '(a b) '(a b)) 'a 'b)
    (if (equal? '(a b) '(a b)) 'a 'b)
    #t #t #f #f
    ))

(define test-expr-y
  '(list 
    (quote (a b d)) 
    '(z e f) 
    (cons 'z '(b c d)) 
    (cons 'a '(b z d)) 
    (if (equal? '(a b) '(a c)) 'a 'b) 
    (if (equal? '(a b) '(a b)) 'z 'b) 
    (if (equal? '(a b) '(a b)) 'a 'z) 
    #t #f #t #f 
    ))