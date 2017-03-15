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
            (if (eq? (type (car string)) type-string)
               (print (car string)) ; TEMP
            (for-each (lambda (s)
                         (display s) (display " "))
               (car string)))
            ;(print (car string))
            (if (not (null? (cdr string))) (loop (cdr string) (+ y 1))))
         (mail sender 'ok)
         (this strings)))))))


; --- word wrapping algorithm ----------------
(define (get-one-word start)
(let loop ((chars #null) (end start))
   (let ((char (car end)))
      (if (has? (list #\space #\newline #\return) char)
         (values (reverse chars) (cdr end))
         (loop (cons char chars) (force (cdr end)))))))

(define (get-all-words string)
(let loop ((words #null) (start (str-iter string)))
   (let* ((word next (get-one-word start)))
      (if (null? next)
         (reverse (cons (runes->string word) words))
         (loop (cons (runes->string word) words) (force next))))))

(define (get-one-line words n)
(let loop ((line #null) (words words) (i 0))
   (let*((word (car words))
         (len (string-length word)))
      (if (> (+ i len) n 1)
         (values (reverse line) words)
         (if (null? (cdr words))
            (values (reverse (cons word line)) #null)
            (loop (cons word line) (cdr words) (+ i len 1)))))))

(define (get-all-lines words n)
(let loop ((lines #null) (words words))
   (let* ((line words (get-one-line words n)))
      (if (null? words)
         (reverse (cons line lines))
         (loop (cons line lines) words)))))

(define (hyphenation width string)
(let*((words (get-all-words string))
      (lines (get-all-lines words width)))
   lines))
; ---


(define (say . args)
   (for-each (lambda (arg)
                (interact 'scrollbox (hyphenation 78 (string-append arg " "))))
      (if (symbol? (car args)) (cdr args) args))
   (interact 'scrollbox '("")))
