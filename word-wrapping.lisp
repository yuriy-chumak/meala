; --- hyp algorithm ----------------
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
            (reverse (cons rout words)))
   
         ((or (grab mask xgg)
              (grab mask xgs)
              (grab mask xsg)
              (grab mask xss))
            (let ((new-word (cons (car word) rout)))
               (walk (cons new-word words) #null  (cdr word) (cdr mask))))
         ((or (grab mask gssssg)
              (grab mask gsssg))
            (let ((new-word (append (reverse (take word 3)) rout)))
               (walk (cons new-word words) #null (cdddr word) (cdddr mask))))
   
         ((or (grab mask sgsg)
              (grab mask gssg)
              (grab mask sggg)
              (grab mask sggs))
            (let ((new-word (append (reverse (take word 2)) rout)))
               (walk (cons new-word words) #null (cddr word) (cddr mask))))
   
         (else
            (walk words (cons (car word) rout) (cdr word) (cdr mask)))))))
; ---

(define text "Так тихо. Не чути нічого. Я не чую навіть свого дихання, і це так дивно. А ще тут темно. Тихо і темно. Темрява плавно огортає мене з усіх сторін, перебирає тонкими пальцями моє волосся, заплітаючи дивні зачіски, закриває холодною долонею очі. Я плаваю у ній, неначе у воді.")

(print "{0---------1+++++++++2---------3+++++++++4---------5+++++++++6---------7+++++++++}")
;(let*((a b (get-one-word (str-iter text))))
;   (print (runes->string a)))


(define (hyphenation n text)
   (define (next iterator)
      (let ((next (cdr iterator)))
         (if (null? next)
            next
            (force next))))
   
   (define (get-word-with-length iter delimiter)
      (let loop ((n 0) (word #null) (iter iter))
         (cond
            ((null? iter)
               (values n (reverse word) iter))
            ((eq? (car iter) delimiter)
               (values n (reverse word) iter))
            (else
               (loop (+ n 1) (cons (car iter) word) (next iter))))))
   
   (define (get-one-line iterator n)
      (let loop ((line #null) (iterator iterator) (i 0))
         (cond
            ((null? iterator) ; строка закончилась
               ;(print "eol")
               ;(print "line: " line)
               ;(print "rev: " (reverse line))
               (values (reverse line) iterator))
            ((eq? (car iterator) #\space)  ; let's skip spaces
               (loop line (next iterator) i))
            ; else
            (else
               (let* ((len word iter (get-word-with-length iterator #\space)))
                  (cond
                     ((> (+ i len) n)
                        ; вот тут у нас есть возможность вставить часть слова с переносом
                        ; (values (reverse line) iterator))
                        ;(print "hyp: " (hyphenate word))
                        (let cycle ((j 1) (hyps (hyphenate word)) (word #null))
                           (let ((len (length (car hyps))))
                           (cond
                              ((> (+ i j len) n)
                                 ;(print "ok:" j)
                                 ;(print "word: " word " > (" (runes->string word) ")")
                                 (if (null? word)
                                    (values (reverse line) iterator)
                                    (let sub ((j j) (iterator iterator))
                                       (if (eq? j 1)
                                          (values (reverse (cons (reverse (cons #\- word)) line)) iterator)
                                          (sub (- j 1) (force (cdr iterator)))))))
                              (else
                                 (cycle (+ j len) (cdr hyps) (append (car hyps) word)))))))


                        ;(todo: если строка пустая, всегда брать первый слог?)
                     (else
                        (loop (cons word line) iter (+ i len 1)))))))))
   
   (let loop ((lines #null) (iterator (str-iter text)))
      (let*((line iterator (get-one-line iterator n)))
         (if (null? iterator)
            (reverse (cons line lines))
            (loop (cons line lines) iterator)))))

#|
(define limits "+++++++++++++++++")
(print limits)
(print (hyphenation (string-length limits) ""))

(for-each (lambda (line)
             (let loop ((s line))
                (if (not (null? s))
                   (begin
                      (display (runes->string (car s)))
                      (if (not (null? (cdr s)))
                         (begin
                            (display (runes->string '(#\space)))
                            (loop (cdr s)))))))
                   (print)) ; не нужен
    (hyphenation (string-length limits) "Так тихо. Не чути нічого. Я не чую навіть свого дихання, і це так дивно. А ще тут темно. Тихо і темно. Темрява плавно огортає мене з усіх сторін, перебирає тонкими пальцями моє волосся, заплітаючи дивні зачіски, закриває холодною долонею очі. Я плаваю у ній, неначе у воді."))
(print "ok.")
|#