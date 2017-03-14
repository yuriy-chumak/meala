(import (lib rlutil))

; константы
(define *scrollbox-position* 42)
(define *scrollbox-size* 20)
(define *map-width* 20)
(define *read_position* 22)

,load "scrollbox.lisp"  ; (say ...)
,load "opengl-map.lisp" ; opengl drawing

; библиотека UI, AI и т.д.
(define :read read)
(define (read)
   (locate 1 *read_position*)
   (display "> ?                                                       ")
   (locate 3 *read_position*)
   (:read))


; --------------------------------------
; -=( разметка )=-----------------------
(define (chapter title)
   (cls)
   (mail 'scrollbox 'clear)
   (set-console-title title))

(define (title text)
   (print text))


(define (map . args)
   (let loop ((args args) (n 1))
      (locate 1 n)
      (print (car args))
      (if (not (null? (cdr args)))
         (loop (cdr args) (+ n 1))))
   (mail 'opengl args))


(define (spawn . args) #t)

; все, можно запускать основной сценарий
,load "scenario.lisp"
