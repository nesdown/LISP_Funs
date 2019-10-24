; Написать программу сортировки [6] списка в соответствии с вариантом в таблице 3
; Шейкер-сортировкa

; Сравнить эффективность реализованной сортировки и реализованного в Задании 1 варианта сортировки Шелла.

; Сортировка двузнаправленная, потому реализуем оба направления по отдельности:

; Аргументы функций:
; arr - список, к которому применяется сортировка
; i - счетчик
; maxVal - максимальное значение
; minVal - минимальное значение
; func - функция, которая передается аргументом зависимо от задачи

; По условию, поэтапно проверяем максимальный элемент, так, что он сдвигается к правому краю - а+1, а+2, ...
; nth - возвращает n-ный элемент списка
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
