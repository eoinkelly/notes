; Clojure
; ;;;;;;;

; clojure runs on the JVM and is nice for concurrent processes
; my-program.clj

; Start the repl
java -cp clojure-1.4.0.jar clojure.main

; Run some basic stuff
(+ 1 2 3)
(println "Hello world")

; show an alert box
(javax.swing.JOptionPane/showMessageDialog nil "Hi")

; Exit the repl
(System/exit 0)