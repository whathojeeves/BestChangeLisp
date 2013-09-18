(defun coinchange_list(total coins lst) 
  (cond
   ((null  coins) nil)
   ((< total 0) nil)
   ((zerop total) (list lst))
   (t (append (coinchange_list total (cdr coins) lst) (coinchange_list (- total (car coins)) coins (append (list (car coins)) lst))))))

(defun getlengths(lst minlst)
 (cond
  
  ((null lst) minlst)
  ((null minlst) (getlengths lst (car lst)))
  ((< (length (car lst)) (length minlst)) (getlengths (cdr lst) (car lst)))
  (t (getlengths (cdr lst) minlst))))

(defun main_solve(total coins)
 (compress (getlengths (coinchange_list total coins '()) '())))

(defun compress (x)   
 (if (consp   x)    
  (compr (car  x) 1 (cdr  x))   
  x))

(defun compr (elt  n  lst)  
 (if (null lst) (list (n-elts elt n))   
  (let ((next (car lst)))   
   (if (eql next elt)  
    (compr elt (+ n  1) (cdr  lst)) 
    (cons (n-elts elt n) 
     (compr next 1 (cdr lst))))))) 

(defun n-elts (elt  n) 
 (if (> n 1) 
  (list n elt)  
  (list 1 elt))) 
