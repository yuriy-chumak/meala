(fork-server 'scrollbox (lambda ()
(let this ((strings #null))
(let* ((envelope (wait-mail))
       (sender msg envelope))
   ; 1. Добавим новую строку к буферу
   (if (eq? msg 'clear)
      ; clear scrollbox window
      (begin
         ;(let loop ((y *scrollbox-size*))
         ;   (locate 42 y)
         ;   (print "                                                                              ")
         ;   (if (less? 0 y)
         ;      (loop (- y 1))))
         (this #null))
      ; else
      (let* ((strings (append strings msg))
             (strings (let loop ((strings strings)) (if (less? (length strings) *scrollbox-size*) strings (loop (cdr strings))))))
         (let loop ((string strings) (y 2))
            (locate *scrollbox-position* y)
            (print "                                                                              ")
            (locate *scrollbox-position* y)
            (print (car string))
            (if (not (null? (cdr string))) (loop (cdr string) (+ y 1))))
         (mail sender 'ok)
         (this strings)))))))

(define (say . args)
   (interact 'scrollbox
      (if (symbol? (car args)) (cdr args) args))
   (interact 'scrollbox '("")))
