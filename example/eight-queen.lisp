(defpackage :eq
  (:shadowing-import-from :series :choose)
  (:use :cl :series :lol))
(in-package :eq)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *opt* '(declare (optimize (speed 3) (space 0) (safety 0))))
  ;;(defvar *opt* "")
  (defvar *benchp* nil)
  )

(defun do-queen ()
  #.*opt*
  (let (head tail (cont #'values))
    (iterate (((curr next)
               (chunk 2 1 (catenate
                           (map-fn
                            t
                            (lambda (ii)
                              #.*opt*
                              (let ((q (queen)))
                                (with-pandoric (i) q (setf i ii))
                                q))
                            (scan-range :length 8))
                           (scan (list nil))))))

      (unless head (setq head curr))
      (unless next (setq tail curr))
      (if next
          (with-pandoric (cont) curr
            (setf cont next))
        (with-pandoric (cont) curr
          (setf cont (lambda (&rest rest) (error "test")
                       ))))
      )

    (plambda (st) (cont)
      #.*opt*
      (flet ((call-cont (st prev-j)
               #.*opt*
               (funcall cont st prev-j)))
        (with-pandoric (cont) tail
          (setf cont #'call-cont))
        (funcall head st -1)))))

(defvar *queen-place-rs* nil
  "return stack. we can implement this as state(argument)")

(defun queen () ;; 8
  (let ((cont (lambda (&rest rest)
                (error rest)
                (values rest)))
        i
        (queen-place-list
          (nreverse (collect (map-fn
                              t
                              (lambda (j)
                                (queen-place j))
                              (scan-range :length
                                          64
                                          ;;#.(if *benchp* 8 64)
                                          ))))))

    (plambda (st prev-j) (cont i)
      #.*opt*
      (flet ((call-cont (st prev-j)
               #.*opt*
               (funcall cont st prev-j)))
        (dolist (qp queen-place-list)
          (push (lambda ()
                  (when (< prev-j (with-pandoric (j) qp j))
                    (with-pandoric (cont) qp
                      (setf cont #'call-cont))
                    (let ((result  (funcall qp (copy-seq st) i)))
                      result)))
                *queen-place-rs*))
        (funcall (pop *queen-place-rs*))))))

(defun queen-place (j) ;; 64
  (let ((cont #'values))
    (plambda (st caller-i) (cont j)
      #.*opt*
      (when (can-place-p st j)
        (funcall cont (new-st st j) j)))))

(declaim (inline i-to-xy xy-to-i))
(defun i-to-xy (i)
  (floor i 8))

(defun xy-to-i (x y)
  (+ (* 8 x) y))

(defun series-nconc (obj)
  "easy implementation of series mapcan like function.
   ex: (series-can #z((1 2 3) (10 20)) => #z(1 2 3 10 20)
   it is not optimized."
  (producing
   (out)
   ((in obj)
    (buffer nil))
   (loop
     (tagbody
        (when buffer (go eatbuf))
        (setq buffer (next-in in (terminate-producing)))
        (if buffer
            (go eatbuf)
          (go end))
      eatbuf
        (next-out out (car buffer))
        (setq buffer (cdr buffer))
        (go end)
      end
        ))))

(defun can-place-p (st i)
  #.*opt*
  (when (svref st i)
    (return-from can-place-p))
  ;; slow and idiot logic.
  (multiple-value-bind (x y) (i-to-xy i) ;;
    (let ((cannot-place-p
            (collect-or
             (mapping ((xy
                        (series-nconc
                         (mapping ((diff (scan-range :from 1 :upto 7)))
                           (list (cons (+ x diff) (+ y diff))
                                 (cons (+ x diff) (- y diff))
                                 (cons (- x diff) (- y diff))
                                 (cons (- x diff) (+ y diff))
                                 (cons x (+ y diff))
                                 (cons x (- y diff))
                                 (cons (+ x diff) y)
                                 (cons (- x diff) y)
                                 )))))
               (let ((xx (car xy))
                     (yy (cdr xy)))
                 (and (<= 0 xx 7)
                      (<= 0 yy 7)
                      (svref st (xy-to-i xx yy))))))))
      (not cannot-place-p)))
  )

(defun new-st (st i)
  #.*opt*
  (let ((nst (copy-seq st)))
    (setf (svref nst i) t)
    nst))

(defun main (&rest argv)
  (declare (ignorable argv))

  (let* ((do-queen (do-queen)))
    (push (lambda () (funcall do-queen (make-array 64 :initial-element nil)))
          *queen-place-rs*))

  (labels ((rep (found-num)
             (let ((rs (pop *queen-place-rs*)))
               (when rs
                 (let ((result (funcall rs)))
                   (if result
                       (progn
                         (format t "------ found-id:~a~%" found-num)
                         (iterate (((a0 a1 a2 a3 a4 a5 a6 a7) (chunk 8 8 (map-fn  t (lambda (x) (if x 1 0)) (scan result)))))
                           (format t "~{~a~^ ~}~%" (list a0 a1 a2 a3 a4 a5 a6 a7)))
                         (rep (1+ found-num)))
                     (rep found-num)))))))
    (rep 0)))
