(defun cocktailFront (arr i maxVal func)
  (cond
    ((>= i maxVal) arr)
    ((< (nth i arr) (nth (+ i 1) arr)) (cocktailFront arr (+ i 1) maxVal nil))
    (t (cocktailFront arr (+ i 1) maxVal (rotatef (nth i arr) (nth (+ i 1) arr))))
  )
)

; Аналогично, смещаем минимальный элемент влево, последовательно.
(defun cocktailBack (arr i minVal func)
  (cond
    ((<= i minVal) arr)
    ((> (nth i arr) (nth (- i 1) arr)) (cocktailBack arr (- i 1) minVal nil))
    (t (cocktailBack arr (- i 1) minVal (rotatef (nth i arr) (nth (- i 1) arr))))
  )
)

; Итеративно вызываем приведенные функции для сортировки
(defun cocktailIter (arr i maxVal)
  (cond
    ((>= i (/ (list-length arr) 2)) arr)
    (t
      (cocktailFront arr i maxVal nil)
      (cocktailBack arr maxVal i nil)
      (cocktailIter arr (+ i 1) (- maxVal 1))
    )
  )
)

; НАКАНЕЦТА!!!
; list-length выдает длинну списка. В остальном, запускаем итератор по данному списку, со счетчиком в 0, и условно взятым максимальным значением в последнем элементе.
; И возвращаем список.
(defun cocktail (arr)
  (cocktailIter arr 0 (- (list-length arr) 1))
  arr
)


; Написать программу объединения двух отсортированных списков в один. При этом порядок сортировки в списке-результате должен сохраняться.

; Самый простой вариант - вернуть объединенный список, повторно отсортированный. Но это вариант для слабаков.
; (defun merger(list1 list2)
;   (cocktail(append(list1 list2)))
; )

; Первое, что нужно проверить - это в каком из списков элементы больше.
; Для этого мы сравним первые элементы каждого из списков, и вернем булевое значение.
; Зависимо от этого, мы вызовем функцию сортировки из предварительно написанных, и вернем итоговый список.

(defun merger (list1 list2)
  (cond
    ((and (null list1) (null list2)) nil)
    ((null list1) (cons (car list2) (merger list1 (cdr list2))))
    ((null list2) (cons (car list1) (merger (cdr list1) list2)))
    ((< (car list1) (car list2)) (cons (car list1) (merger (cdr list1) list2)))
    (T (cons (car list2) (merger list1 (cdr list2))))
  )
)

; (defun merger (list1 list2)
;   (cond(
;       ((and (< (first list1) (second list1)) (> (first list2) (second list2))) nil)
;       ((< (first list1) (second list1)) (cocktail (append list1 list2)))
;       (t (reverse (cocktail (append list1 list2))))
;     )
;   )
; )

(print (merger '(1 2 3 4 5 6 7) '(2 5 10 11 24)))

