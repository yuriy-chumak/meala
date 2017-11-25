#!/usr/bin/ol

;(define-library (file wavefront)
;(export
;   wavefront:obj-fd->sexp)
(import
   (otus lisp)
   (lang sexp)
   (owl parse))

;(begin

; internal number utilities
(define digit-values
   (list->ff (map (lambda (d i) (cons d i)) (iota 10 #\0) (iota 10 0))))  ;; 0-9

(define (runes->integer digits)
   (fold
      (λ (n digit)
         (+ (* n 10) (getf digit-values digit)))
      0 digits))


(define get-rest-of-line
   (let-parses
      ((chars (get-greedy* (get-byte-if (lambda (x) (not (eq? x #\newline))))))
       (skip (get-imm #\newline)))
      chars))

(define get-comment
   (let-parses
         ((skip (get-imm #\#))
          (skip get-rest-of-line))
      #f))

(define get-sign
   (get-any-of (get-imm #\+) (get-imm #\-) (get-epsilon #\+)))

(define (number-char? c)
   (<= #\0 c #\9))

; reads a dotted (possibly) number from stream: 1, 1.11, -2, -33.45
(define get-integer
   (let-parses
         ((integer (get-greedy+ (get-rune-if number-char?))))
      (runes->integer integer)))
(define get-number
   (let-parses
         ((sign get-sign) ; euther #\+ (default) or #\-
          (integer (get-greedy+ (get-rune-if number-char?))) ; integer part of number (as list)
          (fractional (get-either
                        (let-parses
                              ((skip (get-imm #\.))
                               (integer (get-greedy+ (get-rune-if number-char?))))
                           integer)
                        (get-epsilon #f))))
      (let ((integer (runes->integer integer))
            (fractional (if fractional (/ (runes->integer fractional)
                                          (expt 10 (length fractional)))
                                       0)))
         (if (eq? sign #\+)
            (+ integer fractional)
            (- 0 integer fractional)))))


; materials
(define get-mtllib
   (let-parses
         ((skip (get-word "mtllib" #t))
          (skip (get-greedy+ (get-imm #\space)))
          (filename get-rest-of-line))
      (runes->string filename)))

; objects
(define get-vertex
   (let-parses
         ((skip (get-word "v " #t))
          (x get-number)
          (skip (get-imm #\space))
          (y get-number)
          (skip (get-imm #\space))
          (z get-number)
          (skip get-rest-of-line)) ; nothing more to read
      (tuple x y z)))
(define get-normal
   (let-parses
         ((skip (get-word "vn " #t))
          (x get-number)
          (skip (get-imm #\space))
          (y get-number)
          (skip (get-imm #\space))
          (z get-number)
          (skip get-rest-of-line)) ; nothing more to read
      (tuple x y z)))

(define get-face
   (let-parses
         ((skip (get-word "f " #t))
          (a get-integer)
          (skip (get-imm #\/))
          (b (get-either get-integer (get-epsilon #f)))
          (skip (get-imm #\/))
          (c get-integer)
          (skip get-rest-of-line))
      (tuple a b c)))


(define get-object-part
   (let-parses
         ((skip (get-word "usemtl " #t))
          (material get-rest-of-line)
          (faces (get-greedy+ get-face)))
         ; there are can be lines "l " and points "p ", not parsed yet - please remove from file
      (cons
         (runes->string material) ; material name
         faces)))

(define get-object
   (let-parses
         ((skip (get-word "o " #t))
          (name get-rest-of-line)
          (vertices (get-greedy+ get-vertex)) ; v
          (normals (get-greedy+ get-normal))  ; vn
          (parts (get-greedy+ get-object-part))
         ; usemtl ; rendering subparts
   )
      (cons (runes->string name)
            (list->ff `(
               (v . ,vertices)
               (vn . ,normals)
               (usemtl . ,parts))))))


; возвращает пару (имя-файла-с-материалами список-объектов)
(define obj-parser
   (let-parses
         ((skip (get-greedy* get-comment)) ; skip leading comments
          (mtllib get-mtllib)              ; matherial library filename
          (objects (get-greedy+ get-object))) ; complete objects list
      (cons mtllib objects)))

; --------------------------------
; error handling
(define (next-newline-distance lst)
   (let loop ((lst lst) (pos 0))
      (cond
         ((null? lst) (values pos lst))
         ((eq? (car lst) 10) (values (+ pos 1) (cdr lst)))
         (else (loop (cdr lst) (+ pos 1))))))

(define (find-line data error-pos)
   ;(print " - find-line")
   (let loop ((data data) (pos 0))
      ;(print* (list "data " data " pos " pos  " error-pos " error-pos))
      (lets ((next datap (next-newline-distance data)))
         (cond
            ((<= error-pos next)
               (runes->string (take data (- next 1)))) ; take this line
            ((null? data)
               "(end of input)")
            (else
               (loop datap next))))))

(define (syntax-fail pos info lst)
   (print info ">>> " (find-line lst pos) " <<<")
   #false)

; please, remove smoothing groups from file using `sed -i '/^s /d' filename`
; same for lines "l " and points "p "
; additionally we support only triangles! no quads reading available
(define (wavefront:obj-fd->sexp fd)
   (fd->exp-stream fd #false obj-parser syntax-fail))

;))

; *********************************
;(import (file wavefront))

(define scene
(let ((fd (open-input-file "untitled.obj")))
   (if fd
      (let ((sexp (car (wavefront:obj-fd->sexp fd))))
         (close-port fd)
         sexp))))

(print scene)


(print "ok.")