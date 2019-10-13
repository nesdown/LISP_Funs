; Задание 1.  Описать неименованную функцию для объединения голов трех списков в один список, исходные данные взять из таблицы 4.

; Исходные списки:
; 1. (TYPE PRINT DEL)
; 2. (H (H J O) (U J N))
; 3. (READ SAVE LOAD (TXT))

; Исходные данные:
; (TYPE H READ)

(defparameter firstlist '(TYPE PRINT DEL))
(defparameter secondlist '(H (H J O) (U J N)))
(defparameter thirdlist '(READ SAVE LOAD (TXT)))

(print ((lambda(a b c) (list (car a) (car b) (car c))) firstlist secondlist thirdlist))
