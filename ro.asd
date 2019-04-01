#|
  This file is a part of ro project.
  Copyright (c) 2019 fgatherlet (fgatherlet@gmail.com)
|#

#|
  Author: fgatherlet (fgatherlet@gmail.com)
|#

(defsystem "ro"
  :version "0.1.0"
  :author "fgatherlet"
  :license "MIT"
  :depends-on ("let-over-lambda"
               "series"
               "sycamore"
               )
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "char-predicate" :depends-on ("package"))
                 (:file "level0" :depends-on ("char-predicate"))
                 (:file "level1" :depends-on ("level0"))
                 (:file "api0" :depends-on ("level1"))
                 (:file "api1" :depends-on ("api0"))
                 )))
  :description ""
  :long-description
  #.(read-file-string
     (subpathname *load-pathname* "README.markdown"))
  :in-order-to ((test-op (test-op "ro-test"))))
