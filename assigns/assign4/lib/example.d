((if (= (= 5 (+ 1 4)) (= 0 1)) (+ 2 3) (* (+ 4 5) 67)) int IF
  ((= (= 5 (+ 1 4)) (= 0 1)) bool EQ
    ((= 5 (+ 1 4)) bool EQ
      (5 int INTLIT)
      ((+ 1 4) int ADDINT
        (1 int INTLIT)
        (4 int INTLIT)
      )
    )
    ((= 0 1) bool EQ
      (0 int INTLIT)
      (1 int INTLIT)
    )
  )
  ((+ 2 3) int ADDINT
    (2 int INTLIT)
    (3 int INTLIT)
  )
  ((* (+ 4 5) 67) int MULINT
    ((+ 4 5) int ADDINT
      (4 int INTLIT)
      (5 int INTLIT)
    )
    (67 int INTLIT)
  )
)