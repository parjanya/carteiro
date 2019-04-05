#|
  This file is a part of carteiro project.
  Copyright (c) 2019 Edgard Bikelis (bikelis@gmail.com)

  Author: Edgard Bikelis (bikelis@gmail.com)

  Carteiro is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or (at
  your option) any later version.

  Carteiro is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with Carteiro.  If not, see <https://www.gnu.org/licenses/>.
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
