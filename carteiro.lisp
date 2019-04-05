(defpackage carteiro
  (:use :cl)
  (:export rastreio))


(in-package :carteiro)


(defvar *source-site* "https://correiosrastrear.com/?tracking_field=")


(defun get-element-by-class (node class)
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
  (setq str (ppcre:regex-replace-all "[
]" str ""))
  (setq str (ppcre:regex-replace-all "	" str " "))
  (let* ((data (ppcre:all-matches-as-strings "../../...." str))
	 (hora-e-lugar (ppcre:all-matches-as-strings "..:.. [^]*" str))
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
  (let ((out '()))
    (dolist (each (coerce (lquery:$ tabela ".sroDtEvent" (text)) 'list))
      (push (extrair-circunstância each) out))
    out))


;; This relies on the very fortunate whim of the guy who made the site
;; to put the events all inside the <strong> tag.
(defun lista-de-eventos (tabela)
  (reverse (coerce (lquery:$ tabela "strong" (text)) 'list)))


(defun rastreio (código)
  
  (let* ((url (concatenate 'string *source-site* código))
	(arq (drakma:http-request url))
	(arq-parsed (plump:parse arq))
	(tabela (get-element-by-class arq-parsed "listEvent sro")))
  
    (loop for x in (lista-de-circunstâncias tabela)
       for y in (lista-de-eventos tabela)
       collect (concatenate 'list x (list y)))))
