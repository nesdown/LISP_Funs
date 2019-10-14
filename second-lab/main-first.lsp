; Реализовать функцию включения объекта на заданное место в списке (нумерация  элементов – от начала списка).
(defun insertnth (x n l)
  (cond
    ((null l) nil)
    ((< 0  n) (cons (car l) (insertnth x (1- n) (cdr l))))
    ((cons x l))
  )
)

(print (insertnth "HELLO" 5 '(1 2 3 4 5 6 7 8 9 10)))
