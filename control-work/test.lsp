(defun fl(a b c d &optional operator)
 (cond
  ((equal operator "member")
   (cond ((null b) nil)
    ((equal a (car b)) t)
    (t (fl a (cdr b) nil nil "member"))))

  ((equal operator "union")
   (cond ((null a) b) ((null b) a)
    ((fl (car b) a nil nil "member")
     (fl a (cdr b) nil nil "union"))
    (t (cons (car b) (fl a (cdr b) nil nil "union")))))

  ((equal operator "intersect")
   (cond ((null a) nil) ((null b) nil)
    ((fl (car b) a nil nil "member")
     (fl (list (car b)) (fl a (cdr b) nil nil "intersect") nil nil "union"))
    (t (fl a (cdr b) nil nil "intersect"))))

  ((equal operator "except")
   (cond ((null a) nil) ((null b) a)
    ((fl (car a) b nil nil "member") (fl (cdr a) b nil nil "except"))
    (t (fl (list (car a)) (fl (cdr a) b nil nil "except") nil nil "union"))
   ))

  ((null a) nil) ((null operator) (fl a (fl b (fl c d nil nil "intersect") nil nil "union") nil nil "except"))
  )
)

(write (fl '(4 5 12 46) '(6 7 8 9 46 12) '(10 5 20 25) '(4 14 5 9 12)))
