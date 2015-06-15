;; first load the beddb system
(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package :beddb)
  (set-server "http://localhost:8080"))

(defclass shop ()
  ((item :initarg :item)
   (quantity :initarg :quantity)
   (cost :initarg :cost)))

(defun make-shop (item quantity cost)
  (make-instance 'shop :item item :quantity quantity :cost cost))

(defclass cost ()
  ((name :initarg :name)
   (price :initarg :price)))

(defun make-cost (name price)
  (make-instance 'cost :name name :price price))

(defparameter *shop-table*
  '(("apple" 20 2.3) 
    ("orange" 100 3.8)
    ("pear" 200 3.6)
    ("banana" 420 4.5) 
    ("potato" 2456 1.2)))

(defparameter *cost-table*
  '(("apple" 1.5)
    ("orange" 2.4)
    ("pear" 2.2)
    ("banana" 1.5)
    ("potato" 0.6)))

(defun load-data ()
  (progn
    (add-set 'shop)
    (add-set 'cost)
    (mapcar #'save-object
	    (nconc
	     (mapcar (lambda (elems) (apply #'make-shop elems)) *shop-table*)
	     (mapcar (lambda (elems) (apply #'make-cost elems)) *cost-table*)))))

