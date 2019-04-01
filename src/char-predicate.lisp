(in-package :ro)

;;; ------------------------------

(defun compile-compose-or (preds)
  (if (cdr preds)
      `(or (funcall ,(car preds) char)
           ,(compile-compose-or (cdr preds)))
      `(funcall ,(car preds) char)))

(defmacro compose-or (&rest preds)
  `(lambda (char)
     ,(compile-compose-or preds)))

(defun compose-or% (&rest preds)
  (lambda (char)
    (collect-or
     (mapping ((pred (scan preds)))
       (funcall pred char)))))

(defmacro compose-not (pred)
  `(lambda (char) (not (funcall ,pred char))))

;;; ------------------------------

(defun make-char-between-p (char-min char-max)
  (lambda (c)
    (char<= char-min c char-max)))

(defun make-char-in-p (string)
  (lambda (c)
    (find c string)))

;;; ------------------------------

(defun space-p (c)
  "%s. T if c is a whitespace character."
  (or (char= c #\tab)
      (char= c #\space)))

(defun tab-p (c)
  "%t. T if c is a tab character."
  (char= c #\tab))

(defun newline-p (c)
  "%n. T if c is a newline character."
  (or (char= c #\return)
      (char= c #\linefeed)))

;; %a. alpha-char-p
;; %l. lower-case-p
;; %u. upper-case-p
;; %d. digit-char-p

(defun word-char-p (c)
  "%w T if is alphanumeric or an underscore."
  (or (alphanumericp c) (char= c #\_)))

(defun hex-char-p (c)
  "%x. T if c is a hexadecimal character."
  (digit-char-p c 16))

(defun punctuation-p (c)
  "%p. T if c is a punctuation character."
  (find c "`~!@#$%^&*()-+=[]{}\|;:',./<>?\"" :test #'char=))

;;; ------------------------------
(defun make-char-class-predicate (string)
  ;;(error "a")
  (let* ((*target* string)
         (*target-end* (length *target*)))
    (declare (special *target* *target-end*))
    (multiple-value-bind (sp result) (funcall (lex-main) 0 nil)
      (when result
        (let (chars
              preds)
          (iterate ((elm (scan result)))
            (typecase elm
              (character (push elm chars))
              (function (push elm preds))))
          (when (< 0 (length chars))
            (push (make-char-in-p (coerce chars 'string)) preds))
          (when (< 0 (length preds))
            ;; this part should be macro but need make compiler..
            (apply #'compose-or% preds)
            ))))))

(defun lex-escape (char)
  (case char
    (#\s (values #'space-p))
    (#\t (values #'tab-p))
    (#\n (values #'newline-p))
    (#\a (values #'alpha-char-p))
    (#\l (values #'lower-case-p))
    (#\u (values #'upper-case-p))
    (#\d (values #'digit-char-p))
    (#\w (values #'word-char-p))
    (#\x (values #'hex-char-p))
    (#\p (values #'punctuation-p))

    ;; exclusive
    (#\S (values (compose-not #'space-p)))
    (#\T (values (compose-not #'tab-p)))
    (#\N (values (compose-not #'newline-p)))
    (#\A (values (compose-not #'alpha-char-p)))
    (#\L (values (compose-not #'lower-case-p)))
    (#\U (values (compose-not #'upper-case-p)))
    (#\D (values (compose-not #'digit-char-p)))
    (#\W (values (compose-not #'word-char-p)))
    (#\X (values (compose-not #'hex-char-p)))
    (#\P (values (compose-not #'punctuation-p)))

    (t char)
    ))

(defun lex-elm ()
  (let ((cont #'values))
    (plambda (sp result) (cont)
      (let ((rest-sp (- *target-end* sp)))
        (cond
          ((< 0 rest-sp)
           (let ((curr  (char *target* sp))
                 (next  (when (< 1 rest-sp) (char *target* (+ 1 sp))))
                 (nnext (when (< 2 rest-sp) (char *target* (+ 2 sp)))))
             (cond
               ((char= #\% curr)
                (let ((val (lex-escape next)))
                  (funcall cont (+ 2 sp) (cons val result))))
               ((and next
                     (char= #\- next))
                (let ((val (make-char-between-p curr nnext)))
                  (funcall cont (+ 3 sp) (cons val result))))
               (t (funcall cont (+ 1 sp) (cons curr result)))))))))))

(defun lex-many (lex)
  (let ((cont #'values))
    (flet ((walk (sp result)
             (multiple-value-bind (new-sp new-result)
                 (funcall lex sp result)
               (if new-sp
                   (funcall cont new-sp new-result)
                   (funcall cont sp result))
               )))

      (with-pandoric (cont) lex
        (setf cont #'walk))

      (plambda (sp result) (cont)
        (walk sp result)))))

(defun lex-main ()
  (lex-many (lex-elm)))

#|
(let* ((*target* "abcde-f%a")
       (*target-end* (length *target*)))
  (funcall
   (lex-many (lex-elm))
   0 nil))
|#
