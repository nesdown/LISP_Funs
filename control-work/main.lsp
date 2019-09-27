(defun f1(a b c d &optional operator)
  cond (
    ((equal operator "member")
      (cond
        ((null b) nil)
        ((equal a (car b)) t)
        (t (f1 a (cdr b) nil nil "member"))
      )
    )
    ((equal operator "union")
      (cond
        ((null a) b)
        ((null b) a)
        ((f1 (car b) a nil nil "member") (f1 a (cdr b) nil nil "union"))
        (t (cons (car b) (f1 a (cdr b) nil nil "union")))
      )
    )
    ((equal operator "intersection")
      (cond
        ((null a) b)
        ((null b) a)

        ((f1 (car b) a nil nil "member") (f1 (list (car b)) (f1 a (cdr b) nil nil "intersect") nil nil "union"))
      )
    )

(defun intersec(s1 s2)
  (cond
    ((null s1) ())
    ((null s2) ())
    ((memb (car s2) s1) (cons (car s2) (intersec s1 (cdr s2))))
    (t (intersec s1 (cdr s2)))
  )
)

(defun minus(s1 s2)
  (cond
    ((null s1) ())
    ((null s2) s1)
    ((memb (car s1) s2) (minus (cdr s1) s2))
    (t (cons (car s1) (minus (cdr s1) s2)))
  )
)

  )
)
