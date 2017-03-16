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
      (let* ((strings (append strings msg))   ; а теперь ограничим количество отображаемых строк:
             (strings (let loop ((strings strings)) (if (less? (length strings) *scrollbox-size*) strings (loop (cdr strings))))))
         (let loop ((string strings) (y 2))
            (locate *scrollbox-position* y)
            (print "                                                                              ")
            (locate *scrollbox-position* y)
            (if (eq? (type (car string)) type-string)
               #t ;(print (car string)) ; TEMP

               (let print ((word (car string)))
                  (if (not (null? word))
                     (begin
                        (display (runes->string (car word)))
                        (if (not (null? (cdr word)))
                           (begin
                              (display " ")
                              (print (cdr word)))))))
            
;            (for-each (lambda (s)
;                         (display s) (display " "))
;               (car string))


            )
            ;(print (car string))
            (if (not (null? (cdr string)))
               (loop (cdr string) (+ y 1))))
         (mail sender 'ok)
         (this strings)))))))

;(for-each (lambda (line)
;             (let loop ((s line))
;                (if (not (null? s))
;                   (begin
;                      (display (runes->string (car s)))
;                      (if (not (null? (cdr s)))
;                         (begin
;                            (display (runes->string '(#\space)))
;                            (loop (cdr s)))))))
;                   (print)) ; не нужен
;    (hyphenation (string-length limits) "Так тихо. Не чути нічого. Я не чую навіть свого дихання, і це так дивно. А ще тут темно. Тихо і темно. Темрява плавно огортає мене з усіх сторін, перебирає тонкими пальцями моє волосся, заплітаючи дивні зачіски, закриває холодною долонею очі. Я плаваю у ній, неначе у воді."))


; --- word wrapping algorithm ----------------
,load "word-wrapping.lisp"
; ---


(define (say . args)
   (for-each (lambda (arg)
                (interact 'scrollbox (hyphenation 78 arg)))
      (if (symbol? (car args)) (cdr args) args))
   (interact 'scrollbox '("")))
