; seq takes an index n and returns the nth padovan number
(defun seq (n)

(if (< n 3) 1                                 ; if n < 3 return 1
(+ (seq (- n 1)) (seq (- n 2)) (seq (- n 3))) ; else return sum of last 3 entries

) ; endif
) ; defun seq

; sum takes an index n and returns the number of additions required to calculate
; the nth padovan number (with each call to + x y z being two additions)
(defun sums (n)

(cond ((< n 3) 0) ; no addition required for 0,1,2
      ((= n 3) 2) ; 2 additions required for 3
      ((> n 3) (+ (sums (- n 1)) (sums (- n 2)) (sums (- n 3)) 2)) 
      ; recursively find previous sums for > 3

) ; end cond
) ; defun sums

; anon takes an argument tree (one of: a tree, an atom, or an empty list) and returns
; an identical structure with all atoms in the original structure having been converted
; to zero.
(defun anon (tree) 

(cond ((not tree) nil) ; if arg is nil, return nil
      ((atom tree) 0)  ; if arg is atom return atom
      (t (cons (anon (first tree)) (anon (rest tree))))   
                       ; else return ((anon first-part) (anon rest-part))

) ; cond
) ; defun anon
