#!/bin/sh
#|-*- mode:lisp -*-|#
#|
exec ros -Q -- $0 "$@"
|#
(progn ;;init forms
  (ros:ensure-asdf)
  (ql:quickload '(:ro) :silent t)
  )
(in-package :ro)

(defvar global-pc nil)

(defun main (&rest argv)
  (declare (ignorable argv))

  (setf x (let ((cont #'identity))
            (plambda (a) (cont this self)
              (setf global-pc this) ;; preserver pc.
              (format t "x~%")
              (funcall cont a)))
        y (let ((cont #'identity))
            (plambda (a) (cont this self)
              (format t "y~%")
              (funcall cont a))))
  (with-pandoric (cont) x
    (setf cont y))


  (funcall x 1)
  (format t "-----from preserved pc ~%")
  (funcall global-pc 1)

  )

;;(let* ((*target* "abcd-kk%s")
;;       (*target-end* (length *target*)))
;;  (multiple-value-bind (sp result) (funcall (lex-main) 0 nil)
;;    (when result
;;      result)))

