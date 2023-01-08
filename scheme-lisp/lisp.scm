; Resources
; * http://www.schemers.org/
; * https://leanpub.com/fp-oo

; Chicken Scheme
; ;;;;;;;;;;;;;;

; csi = start the REPL
; csc -t myfile.scm # outputs myfile.c
; csc myfile.scm # outputs myfile.so (or tries to on windows anyway)
; Info on the REPL http://wiki.call-cc.org/man/4/Using%20the%20interpreter
; load a file into the REPL
; csi -script file.scm

; Info on scheme & lisp
; http://ocw.mit.edu/courses/electrical-engineering-and-computer-science/6-001-structure-and-interpretation-of-computer-programs-spring-2005/video-lectures/
; http://mitpress.mit.edu/sicp/full-text/book/book.html
; http://en.wikipedia.org/wiki/Scheme_(programming_language)
; http://www.phyast.pitt.edu/~micheles/scheme/index.html
; http://www.ccs.neu.edu/home/dorai/t-y-scheme/t-y-scheme-Z-H-1.html

; The biggest differences are sociological: the Scheme community is more academic
; and interested in research, experimentation and didactic; the Common Lisp
; community is closer to the IT business world and interested in solving real word
; problems.

(print "hello")
(exit) ; exits the REPL

; > Scheme code is not meant to be written by humans, it is intended to be written
; > automatically by macros. Only after having understood this point you will
; > realize that the parentheses are a Good Thing (TM). I needed a few months to
; > understand it, others never understand it and they quit Scheme with disgust.

; form is (operator arg1 arg2)
(+ 1 3)
; 4

; * A Sample Factorial Program

(define (fact n)
  (if (= n 0)
      1
	    (* n (fact (- n 1)))))


(define
	(lambda (l)
		(cond ())
	))
