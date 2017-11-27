#!/usr/bin/ol

;(define-library (file wavefront)
;(export
;   wavefront:obj-fd->sexp)
(import
   (otus lisp)
   (lang sexp)
   (owl parse))

(import
   (owl math fp))

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
         ((skip (get-word "mtllib " #t))
          ;(skip (get-greedy+ (get-imm #\space)))
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
      (list x y z)))
(define get-normal
   (let-parses
         ((skip (get-word "vn " #t))
          (x get-number)
          (skip (get-imm #\space))
          (y get-number)
          (skip (get-imm #\space))
          (z get-number)
          (skip get-rest-of-line)) ; nothing more to read
      (list x y z)))

(define get-face-vertex
   (let-parses
         ((a get-integer)
          (skip (get-imm #\/))
          (b (get-either get-integer (get-epsilon #f)))
          (skip (get-imm #\/))
          (c get-integer))
      (tuple a b c)))

(define get-face
   (let-parses
         ((skip (get-word "f " #t))
          (v1 get-face-vertex)
          (skip (get-imm #\space))
          (v2 get-face-vertex)
          (skip (get-imm #\space))
          (v3 get-face-vertex)
          (skip get-rest-of-line))
      (list v1 v2 v3)))


(define get-object-part
   (let-parses
         ((skip (get-word "usemtl " #t))
          (material get-rest-of-line)
          (skip (get-greedy* (let-parses ; possible "s off"
                                ((skip (get-word "s " #t))
                                 (skip get-rest-of-line)) #f)))
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
   (let loop ((data data) (pos 0))
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
(let ((fd (open-input-file "House.obj")))
   (if fd
      (let ((sexp (car (wavefront:obj-fd->sexp fd))))
         (close-port fd)
         sexp))))
;(print scene)
;,quit

(import (otus ffi))
(import (lib sdl2))

; create OpenGL window:
; ***************************************************
(if (less? (SDL_Init SDL_INIT_VIDEO) 0)
   (begin
      (print "Unable to Init SDL: " (SDL_GetError))
      (exit-owl 1)))

;(unless (eq? (IMG_Init IMG_INIT_PNG) IMG_INIT_PNG)
;   (begin
;      (print "Unable to init SDL png image support: " (SDL_GetError))
;      (exit-owl 1)))

; request OpenGL 3.2
(SDL_GL_SetAttribute SDL_GL_CONTEXT_MAJOR_VERSION 2)
(SDL_GL_SetAttribute SDL_GL_CONTEXT_MINOR_VERSION 0)

(SDL_GL_SetAttribute SDL_GL_DOUBLEBUFFER 1)
(SDL_GL_SetAttribute SDL_GL_DEPTH_SIZE  24)

(define window (SDL_CreateWindow "Obj loading sample"
   SDL_WINDOWPOS_UNDEFINED SDL_WINDOWPOS_UNDEFINED
   640 480 (bor SDL_WINDOW_OPENGL SDL_WINDOW_SHOWN)))
(print window)

(define context (SDL_GL_CreateContext window))
(SDL_GL_SetSwapInterval 1)

; opengl 2.0
(import (OpenGL version-2-0))

; shaders loader:
(define po
(let ((po (glCreateProgram))
      (vs (glCreateShader GL_VERTEX_SHADER))
      (fs (glCreateShader GL_FRAGMENT_SHADER)))
   (if (= po 0)
      (runtime-error "Can't create shader program." '()))

   ; пример, как можно передать в функцию массив указателей на строки:
   ; vertex shader:
   ; http://steps3d.narod.ru/tutorials/lighting-tutorial.html
   (glShaderSource vs 2 (list (c-string "#version 120 // OpenGL 2.1\n")
                              (c-string "
      varying vec3 normal;
      varying vec3 light;
      void main() {
         // gl_Position = gl_Vertex;
         gl_Position = ftransform(); // gl_ModelViewProjectionMatrix * gl_Vertex

         //normal = normalize(gl_Normal).xyz;
         normal = normalize(gl_NormalMatrix * gl_Normal).xyz;

         vec4 p = gl_ModelViewMatrix * gl_Vertex;
         light = normalize(vec3(20, 20, 30) - p.xyz);
      }")) #false)
   (glCompileShader vs)
   (let ((isCompiled (vm:new-raw-object type-vector-raw '(0))))
      (glGetShaderiv vs GL_COMPILE_STATUS isCompiled)

      (if (= (ref isCompiled 0) 0)
         (let*((maxLength "??")
               (_ (glGetShaderiv vs GL_INFO_LOG_LENGTH maxLength))
               (maxLengthValue (+ (ref maxLength 0) (* (ref maxLength 1) 256)))
               (errorLog (make-string maxLengthValue 0))
               (_ (glGetShaderInfoLog vs maxLengthValue maxLength errorLog)))
            (runtime-error errorLog vs))))
   (glAttachShader po vs)

   ; fragment shader:
   (glShaderSource fs 2 (list (c-string "#version 120 // OpenGL 2.1")
                              (c-string "
      // http://glslsandbox.com/e#19102.0
      uniform float time;

      varying vec3 light;
      varying vec3 normal;
      void main(void) {
         //vec2 viewport = vec2(640, 480);

         //get coords and direction
         //vec2 uv=gl_FragCoord.xy / viewport.xy - .5;
         //gl_FragColor = vec4(uv, 0,1);

         //gl_FragColor = vec4(normalize(normal) * 0.7, 1.0);

         vec3 n2   = normalize ( normal );
         vec3 l2   = normalize ( light );
         vec4 diff = vec4(1, 1, 1, 1) * max ( dot ( n2, l2 ), 0.0 );

         gl_FragColor = diff;
      }")) #false)
   (glCompileShader fs)
   (let ((isCompiled (vm:new-raw-object type-vector-raw '(0))))
      (glGetShaderiv fs GL_COMPILE_STATUS isCompiled)

      (if (= (ref isCompiled 0) 0)
         (let*((maxLength "??")
               (_ (glGetShaderiv fs GL_INFO_LOG_LENGTH maxLength))
               (maxLengthValue (+ (ref maxLength 0) (* (ref maxLength 1) 256)))
               (errorLog (make-string maxLengthValue 0))
               (_ (glGetShaderInfoLog fs maxLengthValue maxLength errorLog)))
            (runtime-error errorLog fs))))

   (glAttachShader po fs)

   (glLinkProgram po)
   (glDetachShader po fs)
   (glDetachShader po vs)
))

; *********************************
; INIT
(glClearColor 0 0 0 1)
(glClear (bor GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
(glEnable GL_DEPTH_TEST)
(glEnable GL_CULL_FACE)

; projection
(glMatrixMode GL_PROJECTION)
(glLoadIdentity)
(gluPerspective 55 (/ 640 480) 0.1 100)

; modelview
(glMatrixMode GL_MODELVIEW)
(glLoadIdentity)
(gluLookAt 2 2 3
   0 2 0
   0 1 0)

; xyz
(glUseProgram #f)
(glBegin GL_LINES)
   ; Ox (red)
   (glColor3f 1 0 0)
   (glVertex3f 0 0 0)
   (glVertex3f 2 0 0)
      (glVertex3f 2 0 0)
      (glVertex3f 1.9 0.1 0)
      (glVertex3f 2 0 0)
      (glVertex3f 1.9 0 0.1)
   ; Oy (green)
   (glColor3f 0 1 0)
   (glVertex3f 0 0 0)
   (glVertex3f 0 2 0)
      (glVertex3f 0 2 0)
      (glVertex3f 0.1 1.9 0)
      (glVertex3f 0 2 0)
      (glVertex3f 0 1.9 0.1)
   ; Oz (blue)
   (glColor3f 0 0 1)
   (glVertex3f 0 0 0)
   (glVertex3f 0 0 2)
      (glVertex3f 0 0 2)
      (glVertex3f 0.1 0 1.9)
      (glVertex3f 0 0 2)
      (glVertex3f 0 0.1 1.9)
(glEnd)


   ; we should collect all vertices into one array
; let's compile house to render
(define house (glGenLists 1))

; sample function tlat flatten all lists inside into one
;(define (flatten . args)
;   (reverse
;   (let loop ((tail #null) (args args))
;      (if (null? args)
;         tail
;         (let ((head (car args)))
;            (if (pair? head)
;               (loop (loop tail head) (cdr args))
;               (loop (cons head tail) (cdr args))))))))

(glNewList house GL_COMPILE)
(glBegin GL_TRIANGLES)

(define vertices (list->tuple
   (apply append
      (map (lambda (object)
            (get (cdr object) 'v '()))
         (cdr scene)))))

(define normals (list->tuple
   (apply append
      (map (lambda (object)
            (get (cdr object) 'vn '()))
         (cdr scene)))))

   (for-each (lambda (object)
      (let ((parts (getf (cdr object) 'usemtl)))
         (print "Rendering " (car object))
         ;(print "normals: " normals)
         (for-each (lambda (part)
            ;(print "Using material " (car part))
            (for-each (lambda (face)
               ;(print "face: " face)
               (for-each (lambda (v)
                  (let*((vi unused ni  v)
                        (vertex (ref vertices vi))
                        (normal (ref normals ni)))
                     (glNormal3fv normal)
                     (glVertex3fv vertex)))
                  face))
               (cdr part)))
            parts)))
      (cdr scene))
(glEnd)
(glEndList)

; actual rendering
;(glUseProgram po)
;(let* ((s2 m2 (clock)))
;   (glUniform1f time (+ (/ (- m2 ms) 1000) (- s2 ss))))
;(if (> resolution 0)
;   (glUniform2f resolution width height))

; draw the scene
;(glCallList house)
; finish
;(SDL_GL_SwapWindow window)
;(SDL_Delay 1000)

; render loop
(call/cc (lambda (break)
   (let loop ((alpha 20))
      (let ((event (make-SDL_Event)))
         (let event-loop ()
            (unless (eq? (SDL_PollEvent event) 0)
               (let ((type (int32->ol event 0)))
                  (case type
                     (SDL_QUIT (break))
                     (else
                        #f))
                  (event-loop)))))
      (glClear (bor GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
      (glEnable GL_DEPTH_TEST)
      (glEnable GL_CULL_FACE)

      ; projection
      (glMatrixMode GL_PROJECTION)
      (glLoadIdentity)
      (gluPerspective 55 (/ 640 480) 0.1 100)

      ; modelview
      (glMatrixMode GL_MODELVIEW)
      (glLoadIdentity)
      (let ((x (fmul (fsin (exact->inexact alpha)) (exact->inexact alpha)))
            (z (fmul (fcos (exact->inexact alpha)) (exact->inexact alpha))))
      (gluLookAt x 2 z
         0 2 0
         0 1 0))

      ; house
      (glUseProgram po)
      (glCallList house)


      (SDL_GL_SwapWindow window)
      (loop (+ alpha 0.00012)))))


;SDL_GL_DeleteContext
;SDL_DestroyWindow
;SDL_Quit

(print "ok.")