; Provided under the MIT License: http://mit-license.org/
; Copyright (c) 2020 Andy Kallmeyer <ask@ask.systems>
;
; See lisp/lisp.asm for more details (the top comment).

%include "bootloader/bootsect.asm"

; This is the start for calculating NUM_EXTRA_SECTORS
; There must be nothing other than the bootsector above this label
extra_sectors_start:

%define RUN_CODE ; tell the text editor we have defined run_code
%include "text-editor.asm"
%include "lisp/lisp.asm"

%ifdef DEBUG_TEXT
debug_text:
;db "(define elm2 (lambda (l) (car (cdr l)))) (elm2 '(foo bar baz))", 0
;db "(quote ((l) . (car (cdr l))))",`\n`,'((l) . (car (cdr l)))", 0
;db "(define append1 (lambda (s t) (cond ((pair? s) (cons (car s) (append1 (cdr s) t))) (#t t) ))) (append1 '(this is a) '(test))", 0

;db "(define append1 (lambda (s t)",`\n`
;db "  (cond",`\n`
;db "    ((pair? s) (cons (car s) (append1 (cdr s) t)))",`\n`
;db "    (#t t)",`\n`
;db "  )",`\n`
;db "))",`\n`,`\n`
;db "(define greeting (lambda name",`\n`
;db "   (append1 '(Hello, ) name)",`\n`
;db "))",`\n`,`\n`
;db "(greeting 'lisp)",0

;db "(define append (lambda (t . args)",`\n`
;db "  (cond",`\n`
;db "    ((pair? args) (append1 t (append . args)))",`\n`
;db "    (#t t)",`\n`
;db "  )",`\n`
;db "))",`\n`,`\n`

;db "(define 0 ())",`\n`
;db "(define succ (lambda (x) (cons x 0)))",`\n`
;db "(define 1 (succ 0))"," "
;db "(define 2 (succ 1))"," "
;db "(define 3 (succ 2))",`\n`
;db "(define dec (lambda (x) (car x)))",`\n`
;db "(define rep (lambda (x c)",`\n`
;db "  (cond",`\n`
;db "    ((eq? c 0) ())",`\n`
;db "    (#t (cons x (rep x (dec c)) ))",`\n`
;db "  )",`\n`
;db "))",`\n`
;;db "(rep 'lisp 3)",`\n`,`\n`
;
;db "(define add (lambda (a b)",`\n`
;db "  (cond",`\n`
;db "   ((eq? b 0) a)",`\n`
;db "   (#t (succ (add a (dec b))))",`\n`
;db "  )",`\n`
;db "))",`\n`
;db "(define 5 (add 3 2))",`\n`
;db "(rep 'lisp 5)",0

;db "(define env",`\n`
;; Closures are ((v . x) . e)
;; the global env is set only if there's a defined value in the env
;; So we just need to pop off the defined value and the closure stuff
;db "  (cdr (cdr ((lambda (x) (lambda (y) x)) 'a)))",`\n`
;db ")",`\n`
;db "env",0

db "(define list (lambda args args))",`\n`

db "(define not (lambda (x)",`\n`
db "  (eq? x ())",`\n`
db "))",`\n`,`\n`

;db "(define or1 (lambda (x)",`\n`
;db "  (cond",`\n`
;db "    ((not (pair? x)) x)",`\n`
;db "    ((car x) (car x))",`\n`
;db "    (#t (or1 (cdr x)))",`\n`
;db "  )",`\n`
;db "))",`\n`,`\n`
;
;db "(define or (lambda x",`\n`
;db "  (or1 x)",`\n`
;db "))",`\n`

db "(define orq1 (lambda (x)",`\n`
db "  (cond",`\n`
db "    ((not (pair? x)) x)",`\n`
db "    ((eval (car x)) (eval (car x)))",`\n`
db "    (#t (orq1 (cdr x)))",`\n`
db "  )",`\n`
db "))",`\n`,`\n`

db "(define orq (lambda x",`\n`
db "  (orq1 x)",`\n`
db "))",`\n`

db "(define andq1 (lambda (x)",`\n`
db "  (cond",`\n`
db "    ((eq? x ()) #t)",`\n`
db "    ((not (pair? x)) x)",`\n`
db "    ((not (eval (car x))) ())",`\n`
;db "    ((eq? (cdr x) ()) (eval (car x)))",`\n`
db "    (#t (andq1 (cdr x)))",`\n`
db "  )",`\n`
db "))",`\n`

; I think you could avoid doing this by doing (apply (cons and (cdr x)))
db "(define andq (lambda x",`\n`
db "  (andq1 x)",`\n`
db "))",`\n`,`\n`

;db "(define and1 (lambda (x)",`\n`
;db "  (cond",`\n`
;db "    ((not (pair? x)) x)",`\n`
;db "    ((not (car x)) ())",`\n`
;db "    ((eq? (cdr x) ()) (car x))",`\n`
;db "    (#t (and1 (cdr x)))",`\n`
;db "  )",`\n`
;db "))",`\n`
;
;; I think you could avoid doing this by doing (apply (cons and (cdr x)))
;db "(define and (lambda x",`\n`
;db "  (and1 x)",`\n`
;db "))",`\n`,`\n`

;db "(and 'a)",`\n`
;db "(and1 (cdr '(a b)))",`\n`
;db "(and 'a 'b 'c)",`\n`
;db "(and 'a 'b () 'c)",`\n`
;db "(and1 'a)",`\n`
;db "(andq '(eq? 'b 'b) '(eq? 'a 'a))",`\n`
;db "(or () (eq? 'a 'b) 'd)",`\n`
;db "(or '() '(eq? 'a 'a) '())",`\n`

db "(define inner_equal? (lambda (x y)",`\n`
db "            (andq ",`\n`
db "                (list 'pair? (list 'quote x))",`\n`
db "                (list 'pair? (list 'quote y))",`\n`
db "                (list 'equal? (list 'quote (car x)) (list 'quote (car y)))",`\n`
db "                (list 'equal? (list 'quote (cdr x)) (list 'quote (cdr y)))",`\n`
db "            )",`\n`
db "))",`\n`

db "(define equal?",`\n`
db "    (lambda (x y)",`\n`
db "        (orq",`\n`
db "            (list 'eq? (list 'quote x) (list 'quote y))",`\n`
db "            (list 'inner_equal? (list 'quote x) (list 'quote y))",`\n`
db "        )",`\n`
db "))",`\n`
db "(equal? '((a b) c) '((a b) c) )"

;db "(define if (lambda (x y z)",`\n`
;db "  (cond",`\n`
;db "    (x y)",`\n`
;db "    (#t z)",`\n`
;db "  )",`\n`
;db "))",`\n`
;db "(if (eq? () ()) 'true 'false)",`\n`

db 0

;db "(eq? '(a b c) '(cons a (b c)))",0

;db "(eq? 'a 'a) (eq? 'a 'b) (eq? #t #t) (eq? () ()) (define foo '(test me)) (eq? foo foo) (eq? '(foo) '(foo))", 0
%endif

NUM_EXTRA_SECTORS: equ NUM_SECTORS(extra_sectors_start)
