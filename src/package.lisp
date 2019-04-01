(defpackage ro
  (:shadowing-import-from
   :series
   :choose
   )
  (:import-from
   :sycamore
   :make-tree-map
   :tree-map-insert
   :tree-map-alist
   :tree-map-find
   :tree-map-keys
   )
  (:use :cl :lol :series)
  (:export

   :ro-scan
   :do-ro-scans

   :ro-match
   :ro-split
   :ro-replace

   :$/
   :$/?

   :$lambda

   :$or

   :$char
   :$is
   :$is!

   :$t
   :$s
   :$n
   :$w
   :$t!
   :$s!
   :$n!
   :$w!

   :$[]
   :$[]!

   :$^
   :$$

   :$.

   :${}
   :${}?

   :$*
   :$*?

   :$+
   :$+?

   :$?

   ;; :$trie-or
   :$1

   )
  )
