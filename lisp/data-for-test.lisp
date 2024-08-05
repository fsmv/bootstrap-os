(fake)
want: not-a-function-error

(car x)
want: no-matching-symbol-error

(car ())
want: ERROR

)
want: extra-right-paren-error

(cons 'a 'b)
want: (a . b)

'(a . b)
want: (a . b)

(quote (a . b))
want: (a . b)

'(a b)
want: (a b)

'(a b c d)
want: (a b c d)

(pair? '(a . b))
want: #t

(pair? 'a)
want: ()

(pair? quote)
want: ()

(pair? ())
want: ()

(pair? (lambda (x) (car x)))
want: #t

(pair? (car ()))
want: ()

((lambda (x) x) 'lisp)
want: lisp

((lambda x x) 'lisp)
want: (lisp)

((lambda (x y z) y) 'a 'b 'c)
want: b

((lambda x x) 'a 'b 'c)
want: (a b c)

(lambda (x) (cons x x))
want: (((x) cons x x))

((lambda (x) (cons x x)) 'lisp)
want: (lisp . lisp)

(((lambda (x) (lambda (y) (cons x y))) 'left) 'right)
want: (left . right)

((lambda (fun val) (cons (fun val) (fun val)))
 (lambda (x) (cons x x)) 'lisp)
want: ((lisp . lisp) lisp . lisp)

'((lisp . lisp) . (lisp . lisp))
want: ((lisp . lisp) lisp . lisp)

(car '(#t . ()))
want: #t

(cdr '(#t . ()))
want: ()

(car '(a b c d))
want: a

(cdr '(a b c d))
want: (b c d)

(eq? 'a 'a)
want: #t

(eq? 'a 'b)
want: ()

(eq? 'quote (car (quote '(a b))))
want: #t

(eq? (car (quote '(a b))) 'quote)
want: #t

(eq? (car (quote '(a b))) (car (quote '(a b))))
want: #t

(eq? 'a ())
want: ()

(eq? 'a (lambda (x) x))
want: ()

(eq? 'a car)
want: ()

(eq? 'a #t)
want: ()

(eq? (eq? 'a 'a) #t)
want: #t

(eq? 'ERROR (car ()))
want: #t

(eq? () 'a)
want: ()

(eq? (lambda (x) x) 'a)
want: ()

(eq? car 'a)
want: ()

(eq? () ())
want: #t

(cond (() 'false) (#t 'true))
want: true

(eq? '(a . b) '(a . b))
want: ()

((lambda (x) (eq? x x)) '(foo bar))
want: #t

(cond
  ((eq? 'b #t) 'fail)
  ((eq? 'baz quote) 'foo)
  ((eq? 'a 'a) 'a-match)
  (#t 'last)
)
want: a-match

(cond
  (() 'wtf)
  (quote 'fail)
  (#t 'pass)
)
want: pass
