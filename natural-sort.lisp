(defun make-natural-sorting-key-aux (string off pre-key)
  (declare (optimize (debug 3)))
  (let ((len (length string)))
    (do ((start off (1+ start)))
        ((>= start len) (nreverse pre-key))
      (let ((c (aref string start)))
        (when (digit-char-p c)
          (multiple-value-bind (number end) (parse-integer string :start start :junk-allowed t)
            (return-from make-natural-sorting-key-aux
              (make-natural-sorting-key-aux string end (cons number pre-key)))))
        (when (alpha-char-p c)
          (do ((end (1+ start) (1+ end)))
              ((or (>= end len)
                   (not (alpha-char-p (aref string end))))
               (return-from make-natural-sorting-key-aux
                 (make-natural-sorting-key-aux string end (cons (subseq string start end) pre-key))))))))))

(defun make-natural-sorting-key (string)
  (make-natural-sorting-key-aux string 0 nil))

(defun natural-sorting-key-< (key-a key-b)
  (cond
    ((null key-b) nil)
    ((null key-a) t)
    (t (let ((a (car key-a))
             (b (car key-b)))
         (cond
           ((numberp a) (if (numberp b)
                            (cond
                              ((< a b) t)
                              ((> a b) nil)
                              (t (natural-sorting-key-< (cdr key-a) (cdr key-b))))
                            t))
           ((numberp b) nil)
           ((string< a b) t)
           ((string> a b) nil)
           (t (natural-sorting-key-< (cdr key-a) (cdr key-b))))))))

(defun natural-sorting-key-> (key-a key-b)
  (cond
    ((null key-a) nil)
    ((null key-b) t)
    (t (let ((a (car key-a))
             (b (car key-b)))
         (cond
           ((numberp a) (if (numberp b)
                            (cond
                              ((< a b) nil)
                              ((> a b) t)
                              (t (natural-sorting-key-> (cdr key-a) (cdr key-b))))
                            nil))
           ((numberp b) t)
           ((string< a b) nil)
           ((string> a b) t)
           (t (natural-sorting-key-> (cdr key-a) (cdr key-b))))))))

(defun natural-sort-decorate (x)
  (cons x (make-natural-sorting-key x)))

(defun natural-sort-< (a b)
  (natural-sorting-key-< (cdr a) (cdr b)))

(defun natural-sort-> (a b)
  (natural-sorting-key-> (cdr a) (cdr b)))

(defun natural-sort (list &key key desc stable)
  (let ((cmp (if desc
                 #'natural-sort->
                 #'natural-sort-<))
        (keys (mapcar (if key
                          #'(lambda (x) (natural-sort-decorate (funcall key x)))
                          #'natural-sort-decorate)
                      list)))
    (mapcar #'car
            (if stable
                (stable-sort keys cmp)
                (sort keys cmp)))))

(defun skip-zeros (string offset length)
  (do ((i offset (1+ i)))
      ((or (>= i length)
           (not (eql (aref string i) #\0)))
       i)))

(defun skip-digits (string offset length)
  (do ((i offset (1+ i)))
      ((or (>= i length)
           (not (digit-char-p (aref string i))))
       i)))

(defun skip-alphas (string offset length)
  (do ((i offset (1+ i)))
      ((or (>= i length)
           (not (alpha-char-p (aref string i))))
       i)))

(defun make-natural-sorting-string-key (string)
  (let* ((length (length string))
         (key (make-array (+ length 5)
                          :element-type 'character
                          :fill-pointer 0
                          :adjustable t))
        (offset 0))
    (do ()
        ((>= offset length) (coerce key 'simple-string))
      (block eater
        (let ((c (aref string offset))
              (end))
          (cond
            ((digit-char-p c) (setf offset (skip-zeros string offset length))
                              (setf end (skip-digits string offset length))
                              (do ((digits (- end offset) (- digits 9)))
                                  ((< digits 9) (vector-push-extend (digit-char digits) key))
                                (vector-push-extend #\9 key)))
            ((alpha-char-p c) (setf end (skip-alphas string offset length)))
            (t (incf offset)
               (return-from eater)))
          (do ((i offset (1+ i)))
              ((>= i end))
            (vector-push-extend (aref string i) key))
          (vector-push-extend #\nul key)
          (setf offset end))))))

        
            
           
            

