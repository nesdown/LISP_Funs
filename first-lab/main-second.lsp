; Задание 2.  Описать именованную функцию для создания нового списка из элементов нескольких исходных списков. В качестве исходных списков использовать списки таблицы 4. Номера элементов списков взять в таблице 5.

; Исходные списки:
; 1. (TYPE PRINT DEL)
; 2. (H (H J O) (U J N))
; 3. (READ SAVE LOAD (TXT))

; Порядок элементов:
; 3, 2, 3

(setq firstlist '(TYPE PRINT DEL))
(setq secondlist '(H (HJO) (U J N)))
(setq thirdlist '(READ SAVE LOAD (TXT)))

(defun lister(a b c)
  (list
    (car (cdr (cdr a)))
    (car (cdr b))
    (car (cdr (cdr c)))
  )
)

(print (lister firstlist secondlist thirdlist))
