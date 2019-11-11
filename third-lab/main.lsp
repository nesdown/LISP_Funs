; - создание базы данных;
; − добавление информации в базу данных;
; − модификацию (редактирование) информации;
; − запись базы данных на диск;
; − загрузку базы данных в оперативную память;
; − просмотр информации;
; − удаление информации из базы данных;
; − поиск информации в базе данных;
; − сортировка информации.

; Продукция предприятия. Телевизионные камеры [8]

; Определим нулевую переменную для работы с базой данных - она будет представлять собой "объект". Поскольку переменная глобальная, то название оборачиваем в *.
(defvar *db* nil)

; функция добавления записи с именованными полями
; Поля такие:
; Model - модель товара
; Manufacturer - производитель
; Price - стоимость товара
; Available - доступность товара
(defun make-record (model manufacturer price available)
  (list :model model :manufacturer manufacturer :price price :available available))

; Функция добавления записи
(defun add-record (product) (push product *db*))

; Вывод контента базы данных в приемлимом формате
(defun dump-db ()
  (dolist (product *db*)
    (format t "~{~a:~10t~a~%~}~%" product)))

; Функция, обеспечивающая считывание из консоли
(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

; Функция записи данных о продукте из консоли
(defun prompt-for-product ()
  (make-record
   (prompt-read "Model")
   (prompt-read "Manufacturer")
   (or (parse-integer (prompt-read "Price") :junk-allowed t) 0)
   (y-or-n-p "Available [y/n]: ")))

; Функция рекурсивного добавления товаров на основе ввода из консоли
(defun add-models ()
  (loop (add-record (prompt-for-product))
     (if (not (y-or-n-p "Another? [y/n]: ")) (return))))

; Функция сохранения базы данных в файл
(defun save-db (filename)
  (with-open-file (out filename
                   :direction :output
                   :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* out))))

; Функция загрузки базы данных из файла
(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *db* (read in)))))

; Очистка базы данных. Обнуляет переменную, представляющую базу.
(defun clear-db () (setq *db* nil))

; Используем готовую функцию, которая берет предикат и список, и возвращает только значения, соответствующие предикату. Selector-fn - это наш предикат.
(defun select (selector-fn)
  (remove-if-not selector-fn *db*))

; Здесь мы описываем пакет функций, которые позволяют написать функцию-селектор универсальной, не усложняя процесса разработки.
(defun make-comparison-expr (field value)
  `(equal (getf cd ,field) ,value))

(defun make-comparisons-list (fields)
  (loop while fields
     collecting (make-comparison-expr (pop fields) (pop fields))))

; Определяем макро-функцию, которая выполняет поиск и возврат значений по ключу
(defmacro where (&rest clauses)
  `#'(lambda (cd) (and ,@(make-comparisons-list clauses))))

; Первая версия функции, принимающая вариативный ключ.
; (defun where (&key model manufacturer price (available nil available-p))
;   #'(lambda (cd)
;       (and
;        (if model    (equal (getf cd :model)  model)  t)
;        (if manufacturer   (equal (getf cd :manufacturer) manufacturer) t)
;        (if price   (equal (getf cd :price) price) t)
;        (if available-p (equal (getf cd :available) available) t))))

; Функция обновления записей в базе.
; MAPCAR передвигается по списку, итерируя его, и возвращает необходимые совпадения.
(defun update (selector-fn &key model manufacturer price (available nil available-p))
  (setf *db*
        (mapcar
         #'(lambda (row)
             (when (funcall selector-fn row)
               (if model    (setf (getf row :model) model))
               (if manufacturer   (setf (getf row :manufacturer) manufacturer))
               (if price   (setf (getf row :price) price))
               (if available-p (setf (getf row :available) available)))
             row) *db*)))

; Функция удаления строк из базы, принимая аргументом функцию-селектор.
(defun delete-rows (selector-fn)
  (setf *db* (remove-if selector-fn *db*)))

(defun sorter ()
  (print (sort *db*
  #'string-lessp
  :key #'(lambda (x) (getf x :price))
  )
  )
)

(load-db "cds.asd")
(dump-db)
(sorter)

