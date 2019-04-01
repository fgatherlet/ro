(in-package :ro)

;;; ------------------------------ core api

(defun ro-scan (ro target &key (start 0) (end (length target)))
  (let* ((*target* target)
         (*target-start* start)
         (*target-end* end)
         (st (make-tree-map #'-)) ;; persistent structure to hold captured.
         (match%
           (collect-first
            (choose-if
             (lambda (x) (car x))
             (map-fn
              '(values t t t) ;; matchp from to
              (lambda (start)
                (funcall ro start st)
                )
              (scan-range :from *target-start* :below *target-end*))))))
    (when match%
      (let* ((st (cadr match%))
             (keys (tree-map-keys st))
             (size (length keys))
             (captured (make-array `(,size 2))))

        (iterate ((i (scan-range :length size)))
          (let ((v (tree-map-find st i)))
            (setf (aref captured i 0) (car v)
                  (aref captured i 1) (cdr v))))
        captured
        ))))


(defmacro! do-ro-scans ((captured scan-start o!ro o!target &key start end limit)
                        &body body)
  `(let ((,g!start (or ,start 0))
         (,g!end   (or ,end (length ,g!target)))
         (*target* ,g!target)
         (,g!limit (or (and ,limit) nil)))
     (iterate (((,captured ,scan-start)
                (scan-fn
                 '(values t t t)
                 (lambda ()
                   (values (ro-scan ,g!ro ,g!target :start ,g!start :end ,g!end)
                           ,g!start
                           0
                           ))
                 (lambda (,g!previous-captured ,g!scan-start ,g!i)
                   (declare (ignore ,g!scan-start))
                   (let ((,g!tmp-start (aref ,g!previous-captured 0 1)))
                     (values (ro-scan ,g!ro ,g!target :start ,g!tmp-start :end ,g!end)
                             ,g!tmp-start
                             (+ 1 ,g!i)
                             )))
                 (lambda (,g!previous-captured ,g!scan-start ,g!i)
                   (declare (ignore ,g!scan-start))
                   (or (and ,g!limit
                            (<= ,g!limit ,g!i))
                       (null ,g!previous-captured)))
                 )))
       (declare (ignorable ,captured ,scan-start))
       ,@body))
  )
