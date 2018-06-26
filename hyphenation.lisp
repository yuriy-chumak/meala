#!/usr/bin/ol

; алгоритм П.Христова в модификации Дымченко и Варсанофьева
; https://sites.google.com/site/foliantapp/project-updates/hyphenation
(define change (fold (lambda (state line code)
                        (fold (lambda (state char)
                                 (put state char code))
                           state line))
                  #empty (list
                            (string->runes "йьъ")
                            (string->runes "аеёиоуыэюяaeiіїєouyАЕЁИОУЫЭЮЯAEIІЇЄOUY")
                            (string->runes "бвгджзклмнпрстфхцчшщbcdfghjklmnpqrstvwxzБВГДЖЗКЛМНПРСТФХЦЧШЩBCDFGHJKLMNPQRSTVWXZ"))
                         (list #\x #\g #\s)))

(define xgg    (string->runes "xgg"))
(define xgs    (string->runes "xgs"))
(define xsg    (string->runes "xsg"))
(define xss    (string->runes "xss"))
(define gssssg (string->runes "gssssg"))
(define gsssg  (string->runes "gsssg"))
(define sgsg   (string->runes "sgsg"))
(define gssg   (string->runes "gssg"))
(define sggg   (string->runes "sggg"))
(define sggs   (string->runes "sggs"))

(define (hyphenate word)
(define (grab l p)
   (cond
      ((null? p) l)
      ((null? l) #false)
      ((eq? (car l) (car p))
         (grab (cdr l) (cdr p)))
      (else #false)))
(let*((word word)
      (mask (map (lambda (char)
                    (get change char char))
               word)))
   ;(print (runes->string word))
   ;(print (runes->string mask))
   (let walk ((words #null) (rout #null) (word word) (mask mask))
      (cond
         ((null? mask)
            (reverse (cons (reverse rout) words)))
   
         ((or (grab mask xgg)
              (grab mask xgs)
              (grab mask xsg)
              (grab mask xss))
            (let ((new-word (cons (car word) rout)))
               (walk (cons (reverse new-word) words) #null  (cdr word) (cdr mask))))
         ((or (grab mask gssssg)
              (grab mask gsssg))
            (let ((new-word (append (reverse (take word 3)) rout)))
               (walk (cons (reverse new-word) words) #null (cdddr word) (cdddr mask))))
   
         ((or (grab mask sgsg)
              (grab mask gssg)
              (grab mask sggg)
              (grab mask sggs))
            (let ((new-word (append (reverse (take word 2)) rout)))
               (walk (cons (reverse new-word) words) #null (cddr word) (cddr mask))))
   
         (else
            (walk words (cons (car word) rout) (cdr word) (cdr mask)))))))

;(define (get-word-with-length iter delimiter)
;  (let loop ((n 0) (word #null) (iter iter))
;     (cond
;        ((null? iter)
;           (values n (reverse word) iter))
;        ((eq? (car iter) delimiter)
;           (values n (reverse word) iter))
;         (else
;            (loop (+ n 1) (cons (car iter) word) (next iter))))))

(for-each (lambda (x)
   (print (runes->string x)))
   (hyphenate (string->runes "собака")))

;(define (dosub word limit)
;   (

;(print (hyphenate
;"При работе с текстом часто возникает потребность корректно расставить переносы. Задача на первый взгляд не такая уж очевидная, нужно учитывать особенности каждого языка, чтобы решить, в каком месте разорвать слово."))
(for-each (lambda (x)
   (display (runes->string x)) (print "-"))
   (hyphenate (string->runes "При работе с текстом часто возникает потребность корректно расставить переносы. Задача на первый взгляд не такая уж очевидная, нужно учитывать особенности каждого языка, чтобы решить, в каком месте разорвать слово.")))