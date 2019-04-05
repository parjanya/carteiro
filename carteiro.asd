#|
  This file is a part of carteiro project.
  Copyright (c) 2019 Edgard Bikelis (bikelis@gmail.com)
|#

#|
  Author: Edgard Bikelis (bikelis@gmail.com)
|#

(defsystem "carteiro"
  :version "1"
  :author "Edgard Bikelis <bikelis@gmail.com>"
  :license "MIT"
  :depends-on ("cl-ppcre"
               "drakma"
               "lquery"
               "plump")
  :components ((:file "carteiro"))
  :description "A library for querying the brazilian post office
  tracking system.")
