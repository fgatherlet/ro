
;; ab(%d+)cd(abc|kvg)ef[abcd]gh(?%D*)ij$ 
#|
(quote
 ((:CHAR #\a)
  (:CHAR #\b)
  (:CAPTURE ((:+ (:IS #<FUNCTION DIGIT-CHAR-P>))))
  (:CHAR #\c) (:CHAR #\d)
  (:CAPTURE
   ((:OR ((:CHAR #\a) (:CHAR #\b) (:CHAR #\c))
         ((:CHAR #\k) (:CHAR #\v) (:CHAR #\g)))))
  (:CHAR #\e) (:CHAR #\f) 
  (:IS #<CLOSURE (FLET ANY :IN RE-SET) {10030518EB}>)
  (:CHAR #\g) (:CHAR #\h)
  (:IGNORE
   ((:* (:IS #<CLOSURE (LAMBDA (C) :IN IS-NOT) {10030537FB}>))))
  (:CHAR #\i) (:CHAR #\j) (:END))
 )

ro-is
ro-capture
ro-ignore
ro-not ??
class

|#


;;(defro ro-test (char)
;;  (plambda (sp) (cont self this dup)
;;    (and
;;     (<= sp *target-to*)
;;     (char= (char *target* sp) char)
;;     (funcall cont (1+ sp)))))

;;(let ((x (ro-test #\a))
;;      y)
;;  (with-pandoric (dup) x
;;    (setf y (funcall dup)))
;;  (with-pandoric (cont) x
;;    (setf cont 10))
;;  (with-pandoric (cont) y
;;    (setf cont 20))
;;  
;;  (list
;;   (with-pandoric (cont) x
;;     cont)
;;   (with-pandoric (cont) y
;;     cont)))







;; TODO: want to more general imprementation.
;;(defun dup-plambda (plambda)
;;  (let ((copy (let (cont)
;;                (plambda (sp) (cont self this)
;;                  (error "dummy is called"))))
;;        org-cont org-this)
;;    (with-pandoric (cont this) plambda
;;      (setf org-cont cont
;;            org-this this))
;;    (with-pandoric (cont this) copy
;;      (setf cont org-cont
;;            this org-this))
;;    copy
;;    ))

;;(setf org (let ((cont 10))
;;            (plambda () (cont self this)
;;              cont)))
;;(funcall org)
;;(with-pandoric (cont) org
;;  (setf cont 100))
;;
;;(setf dup (dup-plambda org))
;;(with-pandoric (cont) dup
;;  (setf cont 200))
;;(funcall dup)


;;(LET (THIS SELF)
;;  (SETQ THIS (LAMBDA (A B) XXX)
;;        SELF
;;          (DLAMBDA
;;            (:PANDORIC-GET (LET-OVER-LAMBDA::SYM)
;;             (CASE LET-OVER-LAMBDA::SYM
;;               ((X) X)
;;               (T (ERROR "Unknown pandoric get: ~a" LET-OVER-LAMBDA::SYM))))
;;            (:PANDORIC-SET (LET-OVER-LAMBDA::SYM LET-OVER-LAMBDA::VAL)
;;             (CASE LET-OVER-LAMBDA::SYM
;;               ((X) (SETQ X LET-OVER-LAMBDA::VAL))
;;               (T (ERROR "Unknown pandoric set: ~a" LET-OVER-LAMBDA::SYM))))
;;            (T (&REST LET-OVER-LAMBDA::ARGS)
;;             (APPLY THIS LET-OVER-LAMBDA::ARGS)))))

;;(defro ro-times (just-times ro-pred)
;;  (flet ((walk (sp)
;;           (incf times)
;;           (if (<= times just-times)
;;               (funcall ro-pred sp)
;;             (funcall cont sp))))
;;    (with-pandoric (cont) ro-pred
;;      (setf cont #'walk))
;;    (plambda (sp) (cont self this)
;;      (let ((times 0))
;;        (declare (special times))
;;        (walk sp)))))
;;
;;(defro ro-to-times (max ro-pred)
;;  ;; greedy
;;  (flet ((walk (sp)
;;           (incf times)
;;           ;; (format t "~a times done. max: ~a sp: ~a~%" times max sp)
;;           (and (or (not max)
;;                    (not (< max times)))
;;                (or (funcall ro-pred sp)
;;                    (funcall cont sp)))))
;;    (with-pandoric (cont) ro-pred
;;      (setf cont #'walk))
;;    (plambda (sp) (cont self this)
;;      (let ((times -1))
;;        (declare (special times))
;;        (walk sp)))))
;;
;;(defro ro-to-times-lazy (max ro-pred)
;;  (flet ((walk (sp)
;;           (incf times)
;;           ;; (format t "~a times done. max: ~a sp: ~a~%" times max sp)
;;           (and (or (not max)
;;                    (not (< max times)))
;;                (or (funcall cont sp)
;;                    (funcall ro-pred sp)))))
;;    (with-pandoric (cont) ro-pred
;;      (setf cont #'walk))
;;    (plambda (sp) (cont self this)
;;      (let ((times -1))
;;        (declare (special times))
;;        (walk sp)))))
;;
;;(defun ro-{} (min max ro-pred)
;;  (cond
;;    ((= 0 min)
;;     (ro-to-times max ro-pred))
;;    ((null max)
;;     (ro-seq (ro-times min ro-pred)
;;             (ro-to-times nil (dup-plambda ro-pred))))
;;    ((> min max)
;;     (error "(> min max)"))
;;    ((= min max)
;;     (ro-times min ro-pred))
;;    (t
;;     (ro-seq (ro-times min ro-pred)
;;             (ro-to-times (- max min)
;;                          (dup-plambda ro-pred))))))
;;
;;(defun ro-{}? (min max ro-pred)
;;  (cond
;;    ((= 0 min)
;;     (ro-to-times-lazy max ro-pred))
;;    ((null max)
;;     (ro-seq (ro-times min ro-pred)
;;             (ro-to-times-lazy nil (dup-plambda ro-pred))))
;;    ((> min max)
;;     (error "(> min max)"))
;;    ((= min max)
;;     (ro-times min ro-pred))
;;    (t
;;     (ro-seq (ro-times min ro-pred)
;;             (ro-to-times-lazy (- max min)
;;                               (dup-plambda ro-pred))))))
;;
;;(defun ro-* (ro-pred)
;;  (ro-to-times nil ro-pred))
;;
;;(defun ro-*? (ro-pred)
;;  (ro-to-times-lazy nil ro-pred))
;;
;;
;;(defun ro-+ (ro-pred)
;;  (ro-seq
;;   (ro-times 1 ro-pred)
;;   (ro-to-times nil ro-pred)))
;;
;;(defun ro-+? (ro-pred)
;;  (ro-seq
;;   (ro-times 1 ro-pred)
;;   (ro-to-times-lazy nil ro-pred)))


;;(defun ro-*? (ro-pred)
;;  (ro-to-times-lazy nil ro-pred))
;;
;;
;;(defun ro-+ (ro-pred)
;;  (ro-seq
;;   (ro-times 1 ro-pred)
;;   (ro-to-times nil ro-pred)))
;;
;;(defun ro-+? (ro-pred)
;;  (ro-seq
;;   (ro-times 1 ro-pred)
;;   (ro-to-times-lazy nil ro-pred)))


;;(defro ro-{} (min max ro-pred)
;;  ;; greedy repeat
;;  (flet ((walk (sp)
;;           (incf times)
;;           (cond
;;             ((<= times min)
;;              (funcall ro-pred sp))
;;             ;;((and max (< max times)) nil)
;;             ((or (not max) (< times max)
;;              (or (funcall ro-pred sp)
;;                  (funcall cont sp))))))
;;    (with-pandoric (cont) ro-pred
;;      (setf cont #'walk))
;;    (plambda (sp) (cont self this)
;;      (let ((times -1))
;;        (declare (special times))
;;        (walk sp)))))
;;
;;(defro ro-{}? (min max ro-pred)
;;  ;; greedy repeat
;;  (flet ((walk (sp)
;;           (incf times)
;;           (cond
;;             ((<= times min)
;;              (funcall ro-pred sp))
;;             ((and max (< max times)) nil)
;;             (t
;;              (or (funcall cont sp)
;;                  (funcall ro-pred sp))))))
;;    (with-pandoric (cont) ro-pred
;;      (setf cont #'walk))
;;    (plambda (sp) (cont self this)
;;      (let ((times -1))
;;        (declare (special times))
;;        (walk sp)))))



;;(defun ro-* (ro-pred)
;;  ;; greedy repeat
;;  (flet ((walk (sp)
;;           (or (funcall ro-pred sp)
;;               (funcall cont sp))))
;;    (with-pandoric (cont) ro-pred
;;      (setf cont #'walk))
;;    (plambda (sp) (cont self this)
;;      (walk sp))))
;;
;;
;;(defro ro-*? (ro-pred)
;;  ;; not greedy repeat
;;  (flet ((walk (sp)
;;           (or (funcall cont sp)
;;               (funcall ro-pred sp))))
;;    (with-pandoric (cont) ro-pred
;;      (setf cont #'walk))
;;    (plambda (sp) (cont self this)
;;      (walk sp))))

;;(defro ro-{} (ro-pred)
;;  (with-pandoric (cont) ro-pred ;; ?? better way may be...
;;    (setf cont #'values))
;;  (plambda (sp) (cont self this)
;;    (let ((next-sp (funcall ro-pred sp)))
;;      (if next-sp
;;          (or (funcall cont next-sp)
;;              (funcall this next-sp))
;;        (funcall cont sp)))))



;;; ------------------------------

(defun dump-ro-fn (fn)
  (format t ">>>fn:~s~%" fn)
  (with-pandoric (cont) fn
    (dump-ro-fn cont)))

(defun ro-test (ro target)
  (with-pandoric (cont) ro
    (setf cont #'values))

  (let* ((*target* target)
         (*target-end* (length *target*))
         match-end)
    (setf match-end (funcall ro 0))
    (format t "match-end:~s~%" match-end)))


;;; ----------------------------------------------------

(defun is-not (pred)
  "Create a predicate that tests the inverse."
  #'(lambda (c) (not (funcall pred c))))

;;; ----------------------------------------------------

(defun escape (stream)
  "Return the test and predicate for an escaped character."
  (let ((c (read-char stream)))
    (case c

      ;; user-defined predicate
      (#\: (let ((sym (with-output-to-string (s)
                        (do ((c (read-char stream)
                                (read-char stream)))
                            ((eql c #\:))
                          (write-char c s)))))
             (values :is (read-from-string sym))))

      ;; boundary test
      (#\b (let ((b1 (read-char stream))
                 (b2 (read-char stream)))
             (values :bounds (list b1 b2))))

      ;; named inclusive sets
      (#\s (values :is #'space-p))
      (#\t (values :is #'tab-p))
      (#\n (values :is #'newline-p))
      (#\a (values :is #'alpha-char-p))
      (#\l (values :is #'lower-case-p))
      (#\u (values :is #'upper-case-p))
      (#\d (values :is #'digit-char-p))
      (#\w (values :is #'word-char-p))
      (#\x (values :is #'hex-char-p))
      (#\p (values :is #'punctuation-p))

      ;; named exclusive sets
      (#\S (values :is (is-not #'space-p)))
      (#\T (values :is (is-not #'tab-p)))
      (#\N (values :is (is-not #'newline-p)))
      (#\A (values :is (is-not #'alpha-char-p)))
      (#\L (values :is (is-not #'lower-case-p)))
      (#\U (values :is (is-not #'upper-case-p)))
      (#\D (values :is (is-not #'digit-char-p)))
      (#\W (values :is (is-not #'word-char-p)))
      (#\X (values :is (is-not #'hex-char-p)))
      (#\P (values :is (is-not #'punctuation-p)))

      ;; just a character
      (otherwise (values :char c)))))

(defun make-lexer (stream)
  ;;(with-input-from-string (stream pattern)
  (lambda ()

    (let ((c (read-char stream nil nil)))
      (when c
        (case c

          ;; any character
          (#\. :any)

          ;; escaped characters
          (#\% (escape stream))

          ;; iterators
          (#\* :*)
          (#\+ :+)
          (#\? :?)

          ;; lazy iterator, or end of set
          (#\- (if (eql (peek-char nil stream nil nil) #\])
                   (values :char #\-)
                   (values :-)))

          ;; conditional
          (#\| :or)

          ;; groups
          (#\( (if (eql (peek-char nil stream nil nil) #\?)
                   (values :group (read-char stream))
                   (values :group ())))

          ;; sets
          (#\[ (if (eql (peek-char nil stream nil nil) #\^)
                   (values :set (read-char stream))
                   (values :set ())))

          ;; group and set terminals
          (#\) :end-group)
          (#\] :end-set)

          ;; start boundary
          (#\^ (if (eql (file-position stream) 1)
                   :start
                   (values :char c)))

          ;; end boundary
          (#\$ (if (null (peek-char nil stream nil nil))
                   :end
                   (values :char c)))

          ;; default to just an exact character match
          (otherwise (values :char c)))))))
