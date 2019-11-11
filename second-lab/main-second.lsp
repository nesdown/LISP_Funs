; Написать программу сортировки списка методом Шелла. Вычисление последовательности шагов сортировки производится в соответствии с вариантом в Таблице 2.

; Методом Кнута
; https://gist.github.com/llibra/2948765

; (defparameter 'testList (1750 701 301 132 57 23 10 4 1))

; n - общая длинна аргумента
; h - последовательность Кнута с условием h < n
(defun knuth (n h)
  (cond
    ((< h (/ n 3)) (knuth n (+ (* h 3) 1)))
    (T h)
  )
)

; Обертка функции сортировщика
(defun shell (arr)
  (shellWrapper arr (knuth (list-length arr) 1) (list-length arr)))


; Рекурсивная обертка, работающая с последовательностью Кнута
(defun shellWrapper (arr h n)
  (cond
    ((>= h 1) (shellWrapper (insertion arr h h n) (floor (/ h 3)) n))
    (T arr)
  )
)

; Итерируем массив, запуская рекурсивный insertion-проверку на элементах
(defun insertion (arr h i n)
  (cond
    ((>= i n) arr)
    (T (insertion (insertionLoop arr i h) h (+ i 1) n))
  )
)

; Рекурсивная функция, меняющая местами параметры соответтсвенно проверке
(defun insertionLoop (arr j h)
  (cond
    ((and (>= j h) (< (nth j arr) (nth (- j h) arr)))
      (insertionLoop (swap arr j (- j h)) (- j h) h))
    (T arr)
  )
)

(defun swap (lst i j)
  (swapWrap lst 0 i j (nth i lst) (nth j lst)))

(defun swapWrap (lst current i j val-i val-j)
  (cond
    ((null lst) NIL)
    ((= current i) (cons val-j (swapWrap (cdr lst) (+ current 1) i j val-i val-j)))
    ((= current j) (cons val-i (swapWrap (cdr lst) (+ current 1) i j val-i val-j)))
    (T (cons (car lst) (swapWrap (cdr lst) (+ current 1) i j val-i val-j)))
  )
)

(print (shell '(1750 701 301 89 132 45 57 23 10 4 1)))
