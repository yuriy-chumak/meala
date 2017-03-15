(import 
   (lib opengl)
   (OpenGL version-1-1)
)

(define Context (gl:Create "Meala"))
(import (OpenGL EXT bgra))


(define WIDTH 40)
(define HEIGHT 20)

(gl:Enable Context)
; ----------------------------------
; do some init
   (glMatrixMode GL_PROJECTION)
   (glLoadIdentity) ; тут надо зеркально отразить карту сверху вниз
   (glOrtho -1 (+ WIDTH 1) (+ HEIGHT 3) -3  -1 1)
   (glMatrixMode GL_MODELVIEW)

   (glBindTexture GL_TEXTURE_2D 0)
   (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
   (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)
   (glTexImage2D GL_TEXTURE_2D 0 GL_RGB8
      16 16
      0 GL_BGR GL_UNSIGNED_BYTE (file->vector "ground.rgb"))
   (glDisable GL_TEXTURE_2D)

   (glEnable GL_TEXTURE_2D)
   (glBindTexture GL_TEXTURE_2D 1)
   (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
   (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)
   (glTexImage2D GL_TEXTURE_2D 0 GL_RGB8
      128 128
      0 GL_BGR GL_UNSIGNED_BYTE (file->vector "tileset.rgb"))
(gl:Disable Context)
; ----------------------------------


; ----------------------------------
; функции рисования:
(define (quadT x y u v)
   (glTexCoord2f    u         v)
   (glVertex2f x y)
   (glTexCoord2f    u      (+ v 1/8))
   (glVertex2f x (+ y 1))
   (glTexCoord2f (+ u 1/8) (+ v 1/8))
   (glVertex2f (+ x 1) (+ y 1))
   (glTexCoord2f (+ u 1/8)    v)
   (glVertex2f (+ x 1) y))

(define (safe-at map x y)
   (if (and (<= 0 x 39)
            (<= 0 y 19))
      (string-ref (list-ref map y) x)))

(define (ne? a b) (not (eq? a b)))

(define (draw-map-cell map x y alpha)
(let ((cell (safe-at map x y)))
   (case cell
   (#\~ ; water
      (glColor3f 0 (* alpha 0.4) alpha)
      (quadT x y 6/8 6/8))
   (#\@ ; meala
      (glColor3f alpha alpha alpha)
      (quadT x y 5/8 1/8))

   (#\# ; walls
      (glColor3f alpha alpha alpha)
      (cond
      ; монолитная стена
      ((and (eq? (safe-at map (- x 1) y) #\#)
            (eq? (safe-at map (+ x 1) y) #\#)
            (eq? (safe-at map x (- y 1)) #\#)
            (eq? (safe-at map x (+ y 1)) #\#))
         (quadT x y 3/8 3/8))

      ; прямые стены:
      ; сверху вниз
      ((and (ne? (safe-at map (- x 1) y) #\#)
            (ne? (safe-at map (+ x 1) y) #\#)
            (eq? (safe-at map x (- y 1)) #\#)
            (eq? (safe-at map x (+ y 1)) #\#))
         (quadT x y 3/8 1/8))

      ; слева направо
      ((and (eq? (safe-at map (- x 1) y) #\#)
            (eq? (safe-at map (+ x 1) y) #\#)
            (ne? (safe-at map x (- y 1)) #\#)
            (ne? (safe-at map x (+ y 1)) #\#))
         (quadT x y 1/8 3/8))

      ; углы:
      ; правый-верхний угол
      ((and (eq? (safe-at map (- x 1) y) #\#)
            (ne? (safe-at map (+ x 1) y) #\#)
            (ne? (safe-at map x (- y 1)) #\#)
            (eq? (safe-at map x (+ y 1)) #\#))
         (quadT x y 4/8 2/8))
      ; правый-нижний угол
      ((and (eq? (safe-at map (- x 1) y) #\#)
            (ne? (safe-at map (+ x 1) y) #\#)
            (eq? (safe-at map x (- y 1)) #\#)
            (ne? (safe-at map x (+ y 1)) #\#))
         (quadT x y 4/8 4/8))
      ; левый-верхний угол
      ((and (ne? (safe-at map (- x 1) y) #\#)
            (eq? (safe-at map (+ x 1) y) #\#)
            (ne? (safe-at map x (- y 1)) #\#)
            (eq? (safe-at map x (+ y 1)) #\#))
         (quadT x y 2/8 2/8))
      ; левый-нижний угол
      ((and (ne? (safe-at map (- x 1) y) #\#)
            (eq? (safe-at map (+ x 1) y) #\#)
            (eq? (safe-at map x (- y 1)) #\#)
            (ne? (safe-at map x (+ y 1)) #\#))
         (quadT x y 2/8 4/8))


      ; ответвления:
      ; влево
      ((and (eq? (safe-at map (- x 1) y) #\#)
            (ne? (safe-at map (+ x 1) y) #\#)
            (eq? (safe-at map x (- y 1)) #\#)
            (eq? (safe-at map x (+ y 1)) #\#))
         (quadT x y 2/8 3/8))
      ; вниз
      ((and (eq? (safe-at map (- x 1) y) #\#)
            (eq? (safe-at map (+ x 1) y) #\#)
            (ne? (safe-at map x (- y 1)) #\#)
            (eq? (safe-at map x (+ y 1)) #\#))
         (quadT x y 3/8 4/8))
      ; вверх
      ((and (eq? (safe-at map (- x 1) y) #\#)
            (eq? (safe-at map (+ x 1) y) #\#)
            (eq? (safe-at map x (- y 1)) #\#)
            (ne? (safe-at map x (+ y 1)) #\#))
         (quadT x y 3/8 2/8))
      ; вправо
      ((and (ne? (safe-at map (- x 1) y) #\#)
            (eq? (safe-at map (+ x 1) y) #\#)
            (eq? (safe-at map x (- y 1)) #\#)
            (eq? (safe-at map x (+ y 1)) #\#))
         (quadT x y 4/8 3/8))

      ; тупики:
      ; правый тупик
      ((and (eq? (safe-at map (- x 1) y) #\#)
            (ne? (safe-at map (+ x 1) y) #\#)
            (ne? (safe-at map x (- y 1)) #\#)
            (ne? (safe-at map x (+ y 1)) #\#))
         (quadT x y 6/8 3/8))
      ; левый тупик
      ((and (ne? (safe-at map (- x 1) y) #\#)
            (eq? (safe-at map (+ x 1) y) #\#)
            (ne? (safe-at map x (- y 1)) #\#)
            (ne? (safe-at map x (+ y 1)) #\#))
         (quadT x y 0/8 3/8))
      ; нижний тупик
      ((and (ne? (safe-at map (- x 1) y) #\#)
            (ne? (safe-at map (+ x 1) y) #\#)
            (eq? (safe-at map x (- y 1)) #\#)
            (ne? (safe-at map x (+ y 1)) #\#))
         (quadT x y 3/8 6/8))
      ; верхний тупик
      ((and (ne? (safe-at map (- x 1) y) #\#)
            (ne? (safe-at map (+ x 1) y) #\#)
            (ne? (safe-at map x (- y 1)) #\#)
            (eq? (safe-at map x (+ y 1)) #\#))
         (quadT x y 3/8 0/8))

      ; островок
      ((and (ne? (safe-at map (- x 1) y) #\#)
            (ne? (safe-at map (+ x 1) y) #\#)
            (ne? (safe-at map x (- y 1)) #\#)
            (ne? (safe-at map x (+ y 1)) #\#))
         (quadT x y 3/8 3/8))))

)))


(fork-server 'opengl (lambda ()
(let this ((map #false))
(let ((envelope (check-mail)))
   (if envelope
      (this (ref envelope 2))
      ; regular rendering loop
      (begin
         (gl:ProcessEvents Context)
         (gl:Enable Context)


         (glClearColor 0.2 0.2 0.2 1.)
         (glClear GL_COLOR_BUFFER_BIT)

         ; нарисуем карту как она есть
         (glEnable GL_TEXTURE_2D)
         (glBindTexture GL_TEXTURE_2D 0)

         ; сначала пол
         (glBegin GL_QUADS)
            (glColor3f 0.4 0.4 0.4)
            (glTexCoord2f 0 0)
            (glVertex2f   0 0)
            (glTexCoord2f 0 HEIGHT)
            (glVertex2f   0 HEIGHT)
            (glTexCoord2f WIDTH HEIGHT)
            (glVertex2f   WIDTH HEIGHT)
            (glTexCoord2f WIDTH 0)
            (glVertex2f   WIDTH 0)
         (glEnd)
         (glDisable GL_TEXTURE_2D)

         ; draw map if any
         (if map (begin
            (glEnable GL_TEXTURE_2D)
            (glBindTexture GL_TEXTURE_2D 1)
            (glBegin GL_QUADS)

            (for-each
               (lambda (y)
                  (for-each
                     (lambda (x)
                        (draw-map-cell map x y 1))
                     (iota 40)))
               (iota 20))

            (glEnd)
            (glDisable GL_TEXTURE_2D)
         ))

         ; finish rendering loop
         (gl:SwapBuffers Context)

         ; done.
         (gl:Disable Context)
         (set-ticker-value 0)  ; free this loop

         (this map)))))))
