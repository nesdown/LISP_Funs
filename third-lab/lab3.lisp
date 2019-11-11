#!/usr/bin/env gcl -f
(defvar *db* nil) ; global variable

(defun create-db ()
  (setf *db* nil))

(defun make-record (name type rating price desc)
  (list :name name :type type :rating rating :price price :desc desc))

(defun add-record (record) (push record *db*))

(defun dump-db (a)
  (dolist (record a)
    (format t "~{~a:~10t~a~%~}~%" record)))

(defun save-db (filename)
  (with-open-file (out filename
                  :direction :output
                  :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* out))))

(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *db* (read in)))))

(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun prompt-for-record ()
  (make-record
    (prompt-read "Name")
    (prompt-read "Type")
    (prompt-read "Rating")
    (prompt-read "Price")
    (prompt-read "Desc")))

(defun new-record ()
  (add-record (prompt-for-record)))

(defun match (a b)
  (cond
    ((null a) t)
    ((or
      (equal (getf a (first a)) "*") ; FIXME: error when value is numeric
      (equal (getf b (first a)) (getf a (first a)))
    ) (match (cddr a) b))
    (t nil)
  )
)

(defun search-loop (record db)
  (cond
    ((null db) nil)
    ((match record (first db))
      (dump-db (list (first db)))
      (search-loop record (rest db))
    )
    (t (search-loop record (rest db)))
  )
)

(defun search-repl ()
  (princ "Enter pattern to search by")
  (terpri)
  (setf *search-pattern* (prompt-for-record))
  (terpri)
  (princ "Result:")
  (terpri) (terpri)
  (search-loop *search-pattern* *db*))

(defun update-record (new old)
  (cond
    ((null old) nil)
    ((not (equal (getf new (first old)) "*"))
      (cons (first old) (cons (getf new (first old)) (update-record new (cddr old))))
    )
    (t
      (cons (first old)
        (cons (getf old (first old))
          (update-record new (cddr old))
        )
      )
    )
  )
)

(defun update-loop (pattern record db)
  (cond
    ((null db) nil)
    ((match pattern (first db))
      (cons (update-record record (first db)) (update-loop pattern record (rest db)))
    )
    (t (cons (first db) (update-loop pattern record (rest db))))
  )
)

(defun update-repl ()
  (princ "Enter pattern to update by")
  (terpri)
  (setf *update-pattern* (prompt-for-record))
  (princ "Enter record to update to")
  (terpri)
  (setf *update-record* (prompt-for-record))
  (setf *db* (update-loop *update-pattern* *update-record* *db*)))

(defun delete-loop (pattern db)
  (cond
    ((null db) nil)
    ((match pattern (first db)) (delete-loop pattern (rest db)))
    (t (cons (first db) (delete-loop pattern (rest db))))
  )
)

(defun delete-repl ()
  (princ "Enter pattern to delete by")
  (terpri)
  (setf *db* (delete-loop (prompt-for-record) *db*))
)

(defun getfseq (plist indicator)
  (loop
     for (i v) on plist by #'cddr
     when (string-equal i indicator)
     return v))

(defun less (a b)
  (cond
    ((and (parse-integer a) (parse-integer b)) (< (parse-integer a) (parse-integer b)))
    (t (string< a b)) ; assume strings
  )
)

(defun sort-repl (key)
  (setf *db* (sort *db* `(lambda (a b) (less (getfseq a ,key) (getfseq b ,key)))))
)

(defun help-repl ()
  (princ "
  List of commands:
    ADD - add records to the database
    UPDATE - update records in the database
    DELETE - delete records from the database
    SAVE <file> - save database to <file>
    LOAD <file> - load database from <file>
    SEARCH - search for records in a database
    DUMP - print the database out
    SORT <key> - sort database records by <key>
    EXIT - exit
  ")
  (terpri)
)

(defun dump-repl ()
  (princ "Result:")
  (terpri) (terpri)
  (dump-db *db*)
)

(defun process (query)
  (cond
    ((equal (first query) "EXIT") NIL)
    ((equal (first query) "HELP") (help-repl) t)
    ((equal (first query) "ADD") (new-record))
    ((equal (first query) "SAVE") (save-db (second query)))
    ((equal (first query) "LOAD") (load-db (second query)))
    ((equal (first query) "UPDATE") (update-repl) t)
    ((equal (first query) "DELETE") (delete-repl) t)
    ((equal (first query) "SEARCH") (search-repl) t)
    ((equal (first query) "DUMP") (dump-repl) t)
    ((equal (first query) "SORT") (sort-repl (second query)) t)
    (T
      (princ "Unknown command")
      (terpri)
      t
    )
  )
)

(defun console ()
  (princ ">> ")
  (if (not (process (split (read-line)))) NIL (console)))

(defun split (string)
    (loop for i = 0 then (1+ j)
      as j = (position #\Space string :start i)
      collect (subseq string i j)
      while j))

(princ "Print HELP for help")
(terpri)
(console)
