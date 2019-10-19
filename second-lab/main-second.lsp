; Написать программу сортировки списка методом Шелла. Вычисление последовательности шагов сортировки производится в соответствии с вариантом в Таблице 2.

; Методом Кнута
; https://gist.github.com/llibra/2948765

(defparameter 'testList (1750 701 301 132 57 23 10 4 1))

(defun insertionSort (lst predicate gap)
  (let ((length (length lst)))
    (if (< length 2) lst
      (do ((i 1 (1+ i))) ((eql i length) lst)
        (do ((x (aref lst i))
             (j i (- j gap)))
            ((or (< (- j gap) 0)
                 (not (funcall predicate x (aref lst (1- j)))))
             (setf (aref lst j) x))
          (setf (aref lst j) (aref lst (- j gap))))))))


(defun shell-sort (lst predicate testList)
  (assert (eql 1 (car (last testList))) (testList)
    "Last gap of ~w is not 1." testList)
  (dolist (gap testList lst)
    (insertionSort lst predicate gap)))
