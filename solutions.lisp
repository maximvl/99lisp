(defun my-last (lst)
  (when (consp lst)
      (if (null (cdr lst))
          lst
          (my-last (cdr lst)))))

(defun my-but-last (lst)
  (when (consp lst)
    (if (null (cdr (cdr lst)))
        lst
        (my-but-last (cdr lst)))))

(defun element-at (lst n)
  (when (> n 0)
    (if (= n 1)
        (car lst)
        (element-at (cdr lst) (1- n)))))

(defun my-elements (lst)
  (if (null lst)
      0
      (1+ (my-elements (cdr lst)))))

(defun my-reverse (lst)
  (let ((res nil))
    (dolist (e lst res)
      (setf res (cons e res)))))

(defun palindrome? (lst)
  (equalp lst (my-reverse lst)))

(defun my-flatten (lst)
  (when (consp lst)
    (let ((next (car lst)))
      (if (consp next)
          (append (my-flatten next) (my-flatten (cdr lst)))
          (append (list next) (my-flatten (cdr lst)))))))

(defun compress (lst)
  (when (consp lst)
    (let ((fst (first lst))
          (snd (second lst)))
      (if (equal fst snd) 
          (compress (cdr lst))
          (cons fst (compress (cdr lst)))))))

(defun pack (lst &optional (group (list (car lst))))
  (when (consp lst)
    (let ((fst (first lst))
          (snd (second lst)))
      (cond
        ((equal fst snd) (pack (cdr lst) (cons fst group)))
        (t (cons group (pack (cdr lst) (list snd))))))))

(defun encode (lst)
  (let ((packed (pack lst)))
    (mapcar (lambda (e) (list (my-elements e) (car e))) packed)))

(defun encode-m (lst)
  (let ((packed (pack lst)))
    (mapcar (lambda (e) (let ((len (my-elements e)))
                     (if (= len 1)
                         (car e)
                         (list len (car e))))) packed)))

(defun mklst (size e)
  (when (> size 0)
    (cons e (mklst (1- size) e))))

(defun decode (lst)
  (when (consp lst)
    (let ((fst (car lst)))
      (if (listp fst)
          (append (mklst (car fst) (second fst))
                  (decode (cdr lst)))
          (cons fst (decode (cdr lst)))))))

(defun encode-d (lst &optional (count 1))
  (when (consp lst)
    (cond
      ((equal (car lst) (second lst)) (encode-d (cdr lst) (1+ count)))
      ((= 1 count) (cons (car lst) (encode-d (cdr lst))))
      (t (cons (cons count (car lst)) (encode-d (cdr lst)))))))

(defun dupli (lst)
  (when (consp lst)
    (cons (car lst) (cons (car lst) (dupli (cdr lst))))))

(defun repli (lst times)
  (when (consp lst)
    (append (mklst times (car lst)) (repli (cdr lst) times))))

(defun drop (lst n &optional (counter n))
  (when (consp lst)
    (if (= 1 counter)
        (drop (cdr lst) n)
        (cons (car lst) (drop (cdr lst) n (1- counter))))))

(defun split (lst size)
  (when (consp lst)
    (if (= 1 size)
        (list (list (car lst)) (cdr lst))
        (let ((next (split (cdr lst) (1- size))))
          (cons (cons (car lst) (car next))
                (cdr next))))))

(defun slice (lst n m)
  (when (consp lst)
    (cond
      ((= 0 m) nil)
      ((= 1 n) (cons (car lst) (slice (cdr lst) n (1- m))))
      (t (slice (cdr lst) (1- n) (1- m))))))

(defun rotate (lst n)
  (when (consp lst)
    (let* ((len (length lst))
           (spl (if (> n 0)
                    (split lst n)
                    (split lst (+ len n)))))
      (append (second spl) (car spl)))))

(defun remove-at (lst n)
  (when (consp lst)
    (if (>= 1 n)
        (cdr lst)
        (cons (car lst) (remove-at (cdr lst) (1- n))))))

(defun insert-at (e lst n)
  (when (consp lst)
    (if (= n 1)
        (cons e lst)
        (cons (car lst) (insert-at e (cdr lst) (1- n))))))

(defun range (a b)
  (cond 
    ((= a b) (cons a nil))
    ((> a b) (cons a (range (1- a) b)))
    (t (cons a (range (1+ a) b)))))

(defun rnd-select (lst n)
  (if (zerop n)
      nil
      (let ((pos (1+ (random (length lst)))))
        (cons (element-at lst pos)
              (rnd-select (remove-at lst pos) (1- n))))))

(defun lotto-select (n max)
  (rnd-select (range 1 max) n))

(defun rnd-permu (lst)
  (rnd-select lst (length lst)))

(defun combination (n lst &optional fixed)
  (when (consp lst)
    ()))
