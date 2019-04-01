(in-package :ro)

(defvar *target* nil)
(defvar *target-start* 0)
(defvar *target-end* 1024)
(defvar *captured-start* nil)
(defvar *captured-end* nil)

;;; ------------------------------

(defmacro with-plambda (&body body)
  `(plambda (sp st) (cont self this copy)
     ,@body))

(defmacro! $lambda (&body body)
  `(let (cont copy)
     (setq copy (lambda ()
                  (with-plambda
                    ,@body)))
     (funcall copy)))

(defmacro! defro (name args &body body)
  `(defun ,name (&rest ,g!args)
     (destructuring-bind ,args ,g!args
       (let ((cont #'list)
             (copy (lambda () (apply #',name ,g!args))))
         ,@body))))

(defmacro copy-ro (ro)
  `(with-pandoric (copy) ,ro
     (funcall copy)))

;;; ------------------------------

(defro $char (char)
  (with-plambda
    (and
     (<= (+ sp 1) *target-end*)
     (char= (char *target* sp) char)
     (funcall cont (1+ sp) st))))

(defro $is (cl-lambda)
  (with-plambda
    ;;(let ((char (char *target* sp)))
    (and
     (<= (+ sp 1) *target-end*)
     (funcall cl-lambda (char *target* sp))
     (funcall cont (1+ sp) st))))

(defro $is! (cl-lambda)
  (with-plambda
    ;;(let ((char (char *target* sp)))
    (and
     (<= (+ sp 1) *target-end*)
     (funcall (compose-not cl-lambda) (char *target* sp))
     (funcall cont (1+ sp) st))))

(defun $t () ($is #'tab-p))
(defun $s () ($is #'space-p))
(defun $n () ($is #'newline-p))
(defun $w () ($is #'word-char-p))
(defun $a () ($is #'alpha-char-p))

(defun $t! () ($is! #'tab-p))
(defun $s! () ($is! #'space-p))
(defun $n! () ($is! #'newline-p))
(defun $w! () ($is! #'word-char-p))
(defun $a! () ($is! #'alpha-char-p))

(define-symbol-macro $t ($t))
(define-symbol-macro $s ($s))
(define-symbol-macro $n ($n))
(define-symbol-macro $w ($w))
(define-symbol-macro $a ($a))

(define-symbol-macro $t! ($t!))
(define-symbol-macro $s! ($s!))
(define-symbol-macro $n! ($n!))
(define-symbol-macro $w! ($w!))
(define-symbol-macro $a! ($a!))

(define-symbol-macro $. ($.))


;;; ------------------------------

(defro $[] (string)
  (ro-is (make-char-class-predicate string)))

(defro $[]! (string)
  (ro-is! (make-char-class-predicate string)))

;;; ------------------------------

(defro $^ ()
  (with-plambda
    (and
     (= sp *target-start*)
     (funcall cont sp st))))

(defro $$ ()
  (with-plambda
    (and
     (= sp *target-end*)
     (funcall cont sp st))))

(defro $. ()
  (with-plambda
    (and
     (<= (+ sp 1) *target-end*)
     (funcall cont (1+ sp) st))))

(defro $string (string)
  (let ((string-length (length string)))
    (with-plambda
      ;;(format t "string-length:~s sp:~s *target-end*:~s~%" string-length sp *target-end*)
      (and
       (<= (+ sp string-length) *target-end*)
       (collect-and
        (map-fn
         t
         (lambda (i)
           (char= (char *target* (+ sp i))
                  (char string i)))
         (scan-range :length string-length)))
       (funcall cont (+ sp string-length) st)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun compile-defro-$i (i)
    `(defro ,(symb "$" i) ()
       (with-plambda
         (let* ((cap-start-end (tree-map-find st ,i))
                (cap-start (car cap-start-end))
                (cap-end (cdr cap-start-end))
                (string-length (- cap-end cap-start)))
           (and
            (<= (+ sp string-length) *target-end*)
            (collect-and
             (map-fn
              t
              (lambda (i)
                (char= (char *target* (+ sp i))
                       (char *target* (+ cap-start i))))
              (scan-range :length string-length)))
            (funcall cont (+ sp string-length) st))))))

  (macrolet ((main ()
               `(progn
                  ,@(collect
                        (map-fn
                         t
                         (lambda (i) (compile-defro-$i i))
                         (scan-range :length 10)))
                  ,@(collect
                        (map-fn
                         t
                         (lambda (i)
                           `(define-symbol-macro ,(symb "$" i) (,(symb "$" i))))
                         (scan-range :length 10))))))
    (main))
  )

;;; ------------------------------

(defun compile-pred (pred)
  (etypecase pred
    (string ($string pred))
    (character ($char pred))
    (function pred)))

(defro $/ (&rest ro-preds)
  (let* ((ro-preds (mapcar #'compile-pred ro-preds))
         (head (car ro-preds))
        group-i
         )
    ;; make continuation chain
    (flet ((call-cont (sp-at-group-tail st-ta-group-tail)
             (let* ((group-start (tree-map-find st-ta-group-tail group-i))
                    (nst (tree-map-insert st-ta-group-tail group-i (cons group-start sp-at-group-tail))))
               (funcall cont sp-at-group-tail nst))))
      (iterate (((curr next) (chunk 2 1 (catenate (scan ro-preds) (scan (list #'call-cont))))))
        (with-pandoric (cont) curr
          (setf cont next))))

    ;;;; main
    (with-plambda
      (setq group-i (car (tree-map-keys st)))
      (if group-i
          (incf group-i)
          (setq group-i 0))
      (setq nst (tree-map-insert st group-i sp))
      (funcall head sp nst))))

(defro $/? (&rest ro-preds)
  ;; not capture
  (let* ((ro-preds (mapcar #'compile-pred ro-preds))
         (head (car ro-preds)))
    ;; make continuation chain
    (flet ((call-cont (x st) (funcall cont x st)))
      (iterate (((curr next) (chunk 2 1 (catenate (scan ro-preds) (scan (list #'call-cont))))))
        (with-pandoric (cont) curr
          (setf cont next))))
    ;; create aggregated pred
    (with-plambda
      (funcall head sp st))))


(defro $or (&rest ro-preds)
  (let* ((ro-preds (mapcar #'compile-pred ro-preds)))
    (flet ((call-cont (x st) (funcall cont x st)))
      (iterate ((pred (scan ro-preds)))
        (with-pandoric (cont) pred
          (setf cont #'call-cont))))
    (with-plambda
      (collect-first
       (choose
        (map-fn t (lambda (pred)
                    (funcall pred sp st))
                (scan ro-preds)))))))

;;;;


(defro ${} (min max ro-pred)
  (let ((ro-pred (compile-pred ro-pred)))
    ;; greedy
    (flet ((walk (sp st)
             (incf times) ;; tried times
             (cond
               ((< times min)
                (funcall ro-pred sp st))
               ((and max (< max times)) nil)
               (t (or (funcall ro-pred sp st)
                      (funcall cont sp st))))))
      (with-pandoric (cont) ro-pred
        (setf cont #'walk))
      (with-plambda
        (let ((times -1))
          (declare (special times))
          (walk sp st))))))

(defro ${}? (min max ro-pred)
  (let ((ro-pred (compile-pred ro-pred)))
    ;; lazy
    (flet ((walk (sp st)
             (incf times) ;; tried times
             (cond
               ((< times min)
                (funcall ro-pred sp st))
               ((and max (< max times)) nil)
               (t (or (funcall cont sp st)
                      (funcall ro-pred sp st))))))
      (with-pandoric (cont) ro-pred
        (setf cont #'walk))
      (with-plambda
        (let ((times -1))
          (declare (special times))
          (walk sp st))))))

(defun $* (ro-pred)
  (${} 0 nil ro-pred))

(defun $*? (ro-pred)
  (${}? 0 nil ro-pred))

(defun $+ (ro-pred)
  (${} 1 nil ro-pred))

(defun $+? (ro-pred)
  (${}? 1 nil ro-pred))

(defun $? (ro-pred)
  (${} 0 1 ro-pred))

(defro $trie-or (&rest strings)
  ;; TODO: make
  )

;;; ------------------------------

