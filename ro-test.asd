#|
  This file is a part of ro project.
  Copyright (c) 2019 fgatherlet (fgatherlet@gmail.com)
|#

(defsystem "ro-test"
  :defsystem-depends-on ("prove-asdf")
  :author "fgatherlet"
  :license "MIT"
  :depends-on ("ro"
               "prove")
  :components ((:module "t"
                :components
                ((:test-file "ro"))))
  :description "Test system for ro"

  :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))
