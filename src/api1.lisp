(in-package :ro)

;;; ------------------------------ easy to use api

(defun ro-captured-to-match (target captured)
  (collect
      'vector
    (mapping (((start end) (chunk 2 2 (scan captured))))
      (subseq target start end))))

;; export
(defun ro-match (ro target &key (start 0) (end (length target)))
  (ro-captured-to-match
   target
   (ro-scan ro target :start start :end end)))

;; export
(defmacro! do-ro-matches ((match scan-start ro target &key start end)
                          &body body)
  `(do-ro-scans (,g!captured
                 ,scan-start
                 ,ro ,target
                 ,@(when start `(:start ,start))
                 ,@(when end `(:end ,end)))
     (let ((,match (ro-captured-to-match ,target ,g!captured)))
       ,@body)))

;; export
(defun ro-split (ro target
                 &key
                   (start 0)
                   (end (length target))
                   (limit nil)
                   )
  (let* (result
         (last-captured-match-end start)
         (target-length (length target))
         (limit-to-scan (and limit (1- limit)))
         )
    (do-ro-scans (captured scan-start ro target :start start :end end :limit limit-to-scan)
      (push (subseq target scan-start (aref captured 0 0)) result)
      (setq last-captured-match-end (aref captured 0 1)))
    (push (subseq target last-captured-match-end end) result)
    (nreverse result)
    ))

(defun ro-replace% (ro target replace-fn
                    &key
                      (start 0)
                      (end (length target))
                      (all nil)
                      )
  (let* (result
         (last-captured-match-end start)
         (target-length (length target))
         )
    ;; (push (subseq target 0 start) result)
    (do-ro-scans (captured scan-start ro target :start start :end end)
      (push (subseq target scan-start (aref captured 0 0)) result)
      (push (funcall replace-fn captured) result)
      (setq last-captured-match-end (aref captured 0 1)))
    (push (subseq target last-captured-match-end end) result)
    (format nil "~{~a~}" (reverse result))
    ))

;;(defmacro with-ro-captured (captured &body body)
;;  `(symbol-macrolet
;;       ,(collect (mapping ((i (scan-range :length 10)))
;;                   `(,(intern (format nil "$~d" i) :ro) 
;;                     (subseq *target* (aref ,captured ,i 0) (aref ,captured ,i 1)))))
;;     ,@body))
;;
;;(defmacro! ro-sub-lambda (&body body)
;;  `(lambda (,g!captured)
;;     (declare (ignorable ,g!captured))
;;     (with-ro-captured ,g!captured
;;       ,@body)))


(defmacro! ro-sub-lambda (&body body)
  `(lambda (,g!captured)
     (declare (ignorable ,g!captured))

     (symbol-macrolet
         ,(collect (mapping ((i (scan-range :length 10)))
                     `(,(intern (format nil "$~d" i) :ro)
                       (subseq *target* (aref ,g!captured ,i 0) (aref ,g!captured ,i 1)))))
       ,@body)))


;; export
(defmacro! ro-replace (ro target expression
                          &key start end all) ;; &body body)
  `(ro-replace% ,ro
                ,target
                (ro-sub-lambda ,expression)
                ,@(when start `(:start ,start))
                ,@(when end `(:end ,end))
                ,@(when all `(:all ,all))))

