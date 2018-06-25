#!/usr/bin/ol
(print "\x1B;[8;25;80t")

(import (lib rlutil))

(cls)
(define news (bytes->string (file->list "lib/file/news.txt")))
(print news)

; -- term library ------------------------------------------
(define empty-line (bytes->string (repeat #\space 80)))
(define (term:erase x y n)
   (locate x y)
   (display (substring empty-line 0 (min n (- 80 x)))))
;(define (term:addstr n a s)
;   (let ((len (min
;                  (string-length s)
;                  (if (< n 0) 80 n)
;                  80)))
;      (display (substring s len))))
   ; ...
(define (term:putstr x y n a s)
   (locate x y)
   (set-color a)
   (display s)) ; todo: term:addstr


; -- end of term library ---
(define (note str)
   (term:erase 1 24 255)
   (term:putstr 26 24 -1 WHITE str))
; initial loadings...


(note "[Загрузка размеров массивов...]")
; init_z_info

(note "[Загрузка... (ландшафт)]")
; init_f_info

(note "[Загрузка... (предметы)]")
; init_k_info

(note "[Загрузка... (артефакты)]")
; init_a_info

(note "[Загрузка... (эго-предметы)]")
; init_e_info

(note "[Загрузка... (монстры)]")
; init_r_info

(note "[Загрузка... (лабиринты)]")
; init_v_info

(note "[Загрузка... (история)]")
; init_h_info

(note "[Загрузка... (расы)]")
; init_p_info

(note "[Загрузка... (классы)]")
; init_c_info

(note "[Загрузка... (продавцы)]")
; init_b_info

(note "[Загрузка... (цены)]")
; init_g_info

(note "Загрузка... (цвета)]")
; init_flavor_info


(note "[Запуск... (прочее)]")
; init_other

(note "[Запуск... (память)]")
; init_alloc

(note "[Запуск... (скрипты)]")
; script_init

(note "[Запуск... (загрузка настроек)]")
; (process_pref_file "pref.prf")

(note "[Запуск... (сделано)]")
