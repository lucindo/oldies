;;;
;;; TODO: This needs a little refactoring
;;;
(eval-when (:load-toplevel :compile-toplevel :execute)
  (require :asdf)
  (require :drakma))

(defpackage :beddb
  (:use :common-lisp :drakma)
  (:export #:set-server 
	   #:save-object #:load-object #:delete-object
	   #:add-set #:del-set
	   #:add-index #:del-index
	   #:add-node #:del-node
	   #:add-fragment #:del-fragment))

(in-package :beddb)

(defparameter *beddb-server* nil)
(defparameter *default-external-format* (flex:make-external-format :latin1 :eol-style :lf))

(defun set-server (server)
  (setq *beddb-server*
	(if (string= "/" (subseq server (1- (length server))))
	    server
	  (concatenate 'string server "/"))))
	  
(defun slot-names (object)
  "Returns a list of downcase strings of each object slot's names"
  (mapcar #'string-downcase
	  (mapcar #'sb-mop:slot-definition-name
		  (sb-mop:class-slots (class-of object)))))

(defun slot-values (object)
  "Returns a list of object's slot values"
  (mapcar (lambda (slot) 
	    (slot-value object (sb-mop:slot-definition-name slot)))
	  (sb-mop:class-slots (class-of object))))

(defun record-fields-list (object)
  "Returns a string with a Erlang list of fields of the record 
representation of the object"
  (format nil "[~{~a~^, ~}]" (slot-names object)))

(defun record-name (object)
  "Returns the record name for the object"
  (string-downcase (class-name (class-of object))))

(defun object-to-record (object)
  "Returns a Erlang record internal representation for the object"
  ;; waring: the last space before } on format string is essential!
  ;;         (see the parser of record-to-object)
  (string-downcase
   (format nil "{~a, ~{~s~^, ~} }"
	   (class-name (class-of object))
	   (slot-values object))))

(defun record-to-object (record-string)
  "Retuns an object instance of serialized Erlang record string"
  (multiple-value-bind (class start)
      (read-from-string record-string nil nil :start 1)
    (let ((slot-values ()))
      (loop (multiple-value-bind (value next)
		(read-from-string record-string nil nil :start (1+ start))
	      (when (null value) (return))
	      (setq slot-values (append slot-values (list value)))
	      (setq start next)))
      (let ((object (make-instance class)))
	(mapc (lambda (slot-name value)
		(setf (slot-value object slot-name) value))
	      (mapcar (lambda (slot)
			(sb-mop:slot-definition-name slot))
		      (sb-mop:class-slots (class-of object)))
	      slot-values)
	object))))

;; Stolen from Hunchentoot 0.15.0 by Edi Weitz
(defun url-encode (string &optional (external-format *default-external-format*))
  "URL-encodes a string using the external format EXTERNAL-FORMAT."
  (with-output-to-string (s)
			 (loop for c across string
			       for index from 0
			       do (cond ((or (char<= #\0 c #\9)
					     (char<= #\a c #\z)
					     (char<= #\A c #\Z)
					     ;; note that there's no comma in there - because of cookies
					     (find c "$-_.!*'()" :test #'char=))
					 (write-char c s))
					(t (loop for octet across (flex:string-to-octets string
											 :start index
											 :end (1+ index)
											 :external-format external-format)
						 do (format s "%~2,'0x" octet)))))))


(defun save-object (obj)
  "Saves (insert or update) an object into BedDB. The first slot 
is used as key"
  (let* ((table-name (record-name obj))
	 (url (concatenate 'string *beddb-server* "/db/" table-name)))
    (nth-value 0 (http-request url :method :post :content (object-to-record obj)))))

(defun load-object (class-name id)
  "Load an object from BedDB. Example: (load-object 'class 1) loads the
object of type class with has 1 as the value of first slot."
  (let ((url (concatenate 'string
			  *beddb-server*
			  "/db/"
			  (string-downcase (symbol-name class-name))
			  "/"
			  (url-encode (string-downcase (format nil "~s" id))))))
    (record-to-object (http-request url))))

(defun delete-object (class-name id)
  "Deletes an object from BedDB. Example: (load-object 'class 1) deltes the
object of type class with has 1 as the value of first slot."
  (let ((url (concatenate 'string
			  *beddb-server*
			  "/db/"
			  (string-downcase (symbol-name class-name))
			  "/"
			  (url-encode (format nil "~s" id)))))
    (nth-value 0 (http-request url :method :delete))))

(defun get-url-for-command (class command &key (end-slash-p nil))
  (concatenate 'string
	       *beddb-server*
	       "/meta/"
	       command
	       "/"
	       (string-downcase (symbol-name class))
	       (when end-slash-p "/")))

(defun add-set (class)
  "Creates a set for class on BedDB"
  (let ((obj (make-instance class))
	(url (get-url-for-command class "set")))
    (nth-value 
     0 
     (http-request url :method :post :content (record-fields-list obj)))))

(defun del-set (class)
  "Deletes a set of class from BedDB"
  (nth-value 0 (http-request (get-url-for-command class "set") 
			     :method :delete)))

(defun add-index (class slot)
  "Creates an index for slot of class on BedDB. The arguments must be symbols"
  (nth-value
   0
   (http-request (concatenate 'string 
			      (get-url-for-command class "index" :end-slash-p t)
			      (string-downcase (symbol-name slot)))
		 :method :put :content "")))

(defun del-index (class slot)
  "Creates an index for slot of class on BedDB. The arguments must be symbols"
  (nth-value
   0
   (http-request (concatenate 'string 
			      (get-url-for-command class "index" :end-slash-p t)
			      (string-downcase (symbol-name slot)))
		 :method :delete)))

(defun add-fragment (class)
  "Adds a fragment on class' set"
  (nth-value 0 (http-request (get-url-for-command class "fragment") 
			     :method :put :content "")))

(defun del-fragment (class)
  "Delete a fragment on class' set"
  (nth-value 0 (http-request (get-url-for-command class "fragment") 
			     :method :delete)))

(defun add-node (class node)
  "Adds a node for class on BedDB. The arguments must be symbols"
  (nth-value
   0
   (http-request (concatenate 'string 
			      (get-url-for-command class "node" :end-slash-p t)
			      (string-downcase (symbol-name node)))
		 :method :put :content "")))

(defun del-node (class node)
  "Deletes a node from class on BedDB. The arguments must be symbols"
  (nth-value
   0
   (http-request (concatenate 'string 
			      (get-url-for-command class "node" :end-slash-p t)
			      (string-downcase (symbol-name node)))
		 :method :delete)))

(defgeneric compare-objects (object1 object2)
  (:documentation "compare each slots of arguments with equal")
  (:method (object1 object2)
	   (and (equal (class-of object1)
		       (class-of object2))
		(reduce (lambda (b1 b2) (and b1 b2))
			(mapcar (lambda (value1 value2)
				  (equal value1 value2))
				(slot-values object1)
				(slot-values object2))))))

#|

Old implementation 
(dont use the Erlang internal representation of records)

(defun record-definition (object)
  "Returns a string with the internal representation (tuple) of a 
Erlang record for the object, someting like {class_name, undefined, ...}"
  (format nil "{~a, ~{undefined~*~^, ~}}"
	  (string-downcase (class-name (class-of object)))
	  (slot-names object)))

(defun slots (object)
  "Returns a list of pairs. Each pair has the slot name (donwcase
string) as first element and the slot value as second"
  (mapcar #'list
	  (slot-names object)
	  (slot-values object)))

(defun object-to-record (object)
  (format nil "#~a{~{~{~#[~;~s~:;~a=~]~}~^, ~}}"
	  (string-downcase (class-name (class-of object)))
	  (slots object)))

(defun record-definition (object)
  (format nil "-record(~a, {~{~a~^, ~}})"
	  (string-downcase (class-name (class-of object)))
	  (slot-names object)))

;;; Tests

(defclass point ()
  ((x :initarg :x)
   (y :initarg :y)))

(defmethod print-object ((object point) stream)
  (format stream "#<point :x ~s :y ~s>" 
	  (slot-value object 'x)
	  (slot-value object 'y)))

(defclass point3d (point)
  ((z :initarg :z)))
|#