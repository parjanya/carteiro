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

(defpackage carteiro
  (:use :cl)
  (:export rastreio))


(in-package :carteiro)


(defvar *source-site* "https://correiosrastrear.com/?tracking_field=")


(defun get-element-by-class (node class)
  "Accepts the DOM root element (a PLUMP-DOM:ROOT object) containing the DOM representation of the page which contains the list of events.
Returns a DOM:ELEMENT holding the table with the list of events."
  (labels ((scanren (node)
             (loop for child across (plump:children node)
                   do (when (plump:element-p child)
                        (let ((cid (plump:attribute child "class")))
                          (when (string-equal class cid)
                            (return-from get-element-by-class child)))
                        (scanren child)))))
    (scanren node))
  NIL)


;; This works basically by finding the date (data) as matching
;; ../../...., DD-MM-YYYY, the time (hora) as ..:.., and the place
;; (lugar) by what remains after removing those.
(defun extrair-circunstância (str)
  "Accepts a string (the response from the server) from where date, time, location will
be extracted.
Returns a list containing three strings, with the date, hour and location.
The string MUST be terminated with a slash (/).

For example,
 (extrair-circunstância \"11/12/1900 10:55 NOWHERELAND /\")
==> (\"11/12/1900\" \"10:55\" \"NOWHERELAND /\")
"
  (setq str (ppcre:regex-replace-all "[
]" str ""))
  (setq str (ppcre:regex-replace-all "	" str " "))
  (let* ((data (ppcre:all-matches-as-strings "../../...." str))
	 (hora-e-lugar (ppcre:all-matches-as-strings "..:.. [^
]*" str))
	 (hora (ppcre:all-matches-as-strings "..:.." str))
	 (lugar (ppcre:regex-replace-all
		 (concatenate 'string
			      (car hora)
			      "[ 	]*\(.*[ ]*./[^ ]*\)[ ]*")
		 (car hora-e-lugar)
		 "\\1")))

    ;; the regex for data and hora returns a list; we only know how to
    ;; handle it’s CAR
    (if (eql (length data) 1)
	(setq data (car data)))
     (if (eql (length hora) 1)
	(setq hora (car hora)))
    (list data hora lugar)))


;; Each entry is inside the .sroDtEvent class.
(defun lista-de-circunstâncias (tabela)
  "Accepts a PLUMP-DOM:ELEMENT, representing a table.
Returns a list of lists. Each of the interior lists is an event in the history of the parcel,
containing three strings: date, hour, and location.

Example of output:
((\"27/02/2019\" \"08:39\" \"ESTADOS UNIDOS DA AMÉRICA / \")
 (\"28/02/2019\" \"10:36\" \"ESTADOS UNIDOS DA AMÉRICA / \")
 (\"28/02/2019\" \"14:28\" \"ESTADOS UNIDOS DA AMÉRICA /\")
 (\"01/03/2019\" \"11:20\" \"RIO DE JANEIRO / RJ\")
 (\"06/03/2019\" \"15:55\" \"RIO DE JANEIRO /RJ\")
 (\"11/03/2019\" \"06:50\" \"RIO DE JANEIRO /RJ\")
 (\"11/03/2019\" \"12:50\" \"RIO DE JANEIRO / RJ\")
 (\"11/03/2019\" \"12:50\" \"RIO DE JANEIRO / RJ\")
 (\"16/03/2019\" \"10:38\" \"RIO DE JANEIRO / RJ\")
 (\"16/03/2019\" \"10:40\" \"RIO DE JANEIRO /RJ\")
 (\"19/03/2019\" \"07:19\" \"SAO PAULO /SP\")
 (\"20/03/2019\" \"11:57\" \"SAO PAULO / SP\")
 (\"20/03/2019\" \"15:22\" \"SAO PAULO / SP\"))"
  (let ((out '()))
    (dolist (each (coerce (lquery:$ tabela ".sroDtEvent" (text)) 'list))
      (push (extrair-circunstância each) out))
    out))


;; This relies on the very fortunate whim of the guy who made the site
;; to put the events all inside the <strong> tag.
(defun lista-de-eventos (tabela)
  "Accepts a PLUMP-DOM:ELEMENT representing a table, and extracts the list
of event descriptions, which is returned as a list of strings.
Example of output:
(\"Objeto postado\" \"Objeto recebido na unidade de exportação\"
 \"Objeto encaminhado\" \"Objeto recebido pelos Correios do Brasil\"
 \"Objeto encaminhado\" \"Objeto encaminhado\" \"Aguardando pagamento\"
 \"Aguardando pagamento\" \"Fiscalização Aduaneira finalizada\"
 \"Objeto encaminhado\" \"Objeto encaminhado\"
 \"Objeto saiu para entrega ao destinatário\" \"Objeto entregue ao destinatário\")
"
  (reverse (coerce (lquery:$ tabela "strong" (text)) 'list)))



(defun rastreio (código)
  "Accepts a 13-character tracking code, as a string, and returns a list of events.
Each event is a list containing:
- date
- time
- place
- event description"

  (let* ((url (concatenate 'string *source-site* código))
	(arq (drakma:http-request url))
	(arq-parsed (plump:parse arq))
	(tabela (get-element-by-class arq-parsed "listEvent sro")))
  
    (loop for x in (lista-de-circunstâncias tabela)
       for y in (lista-de-eventos tabela)
       collect (concatenate 'list x (list y)))))
