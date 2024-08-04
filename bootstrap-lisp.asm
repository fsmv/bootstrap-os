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

db "(define 0 ())",`\n`
db "(define succ (lambda (x) (cons x 0)))",`\n`
db "(define 1 (succ 0))"," "
db "(define 2 (succ 1))"," "
db "(define 3 (succ 2))",`\n`
db "(define dec (lambda (x) (car x)))",`\n`
db "(define rep (lambda (x c)",`\n`
db "  (cond",`\n`
db "    ((eq? c 0) ())",`\n`
db "    (#t (cons x (rep x (dec c)) ))",`\n`
db "  )",`\n`
db "))",`\n`
;db "(rep 'lisp 3)",`\n`,`\n`

db "(define add (lambda (a b)",`\n`
db "  (cond",`\n`
db "   ((eq? b 0) a)",`\n`
db "   (#t (succ (add a (dec b))))",`\n`
db "  )",`\n`
db "))",`\n`
db "(define 5 (add 3 2))",`\n`
db "(rep 'lisp 5)",0

;db "(eq? '(a b c) '(cons a (b c)))",0

;db "(eq? 'a 'a) (eq? 'a 'b) (eq? #t #t) (eq? () ()) (define foo '(test me)) (eq? foo foo) (eq? '(foo) '(foo))", 0
%endif

NUM_EXTRA_SECTORS: equ NUM_SECTORS(extra_sectors_start)
