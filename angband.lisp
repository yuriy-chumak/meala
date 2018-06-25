#!/usr/bin/ol
;(print "\x1B;[8;25;80t")

(import (lib rlutil))

(cls)
(define news (bytes->string (file->list "lib/file/news.txt")))
(print news)

; -- term library ------------------------------------------
(define empty-line (runes->string (repeat " " 80)))
(define (term:erase x y n)
   (locate x y)
   (display (substring empty-line (min n (- 80 x)))))
(define (term:addstr n a s)
   ; ...
(define (term:putstr x y n a s)
   (locate x y)
   


; -- end of term library ---
(locate 1 1) (display "x")
(term:erase 1 24 255)
