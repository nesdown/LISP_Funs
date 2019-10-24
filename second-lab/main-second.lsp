; Написать программу сортировки списка методом Шелла. Вычисление последовательности шагов сортировки производится в соответствии с вариантом в Таблице 2.

; Методом Кнута
; https://gist.github.com/llibra/2948765

(defparameter 'testList (1750 701 301 132 57 23 10 4 1))

(defun gap-insertion-sort (array predicate gap)
  (let ((length (length array)))
    (if (< length 2) array
      (do ((i 1 (1+ i))) ((eql i length) array)
        (do ((x (aref array i))
             (j i (- j gap)))
            ((or (minusp (- j gap))
                 (not (funcall predicate x (aref array (1- j)))))
             (setf (aref array j) x))
          (setf (aref array j) (aref array (- j gap))))))))

(defconstant +gaps+
  '(9841 3280 1093 364 121 40 13 4 1)
  "The best sequence of gaps, according to Donald E. Knuth.")

(defun shell-sort (array predicate &optional (gaps +gaps+))
  (assert (eql 1 (car (last gaps))) (gaps)
    "Last gap of ~w is not 1." gaps)
  (dolist (gap gaps array)
    (gap-insertion-sort array predicate gap)))
