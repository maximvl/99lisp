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
