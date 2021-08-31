(ql:quickload :cl-algebraic-data-type)
(use-package :cl-algebraic-data-type)

;; Could BFS superclasses for impl (needs MOP)
;; Typeclasses don't really go with subclasses though (see Haskell):
(defun find-impl (obj tclass)
  (get (class-name  (class-of obj)) tclass))
(defun set-impl (class tclass f impl)
  (flet ((ensure-ht ()
	   (unless (get class tclass)
	     (setf (get class tclass) (make-hash-table)))))
    (ensure-ht)
    (setf (gethash f (get class tclass)) impl)))

(adt:defdata maybe
  (just t)
  nothing)

;; Generate this:

(set-impl 'just 'functor 'fmap
	  (lambda (x f)
	    (match maybe x
		   ((just y)
		    (just (funcall f y)))
		   (_			; Unreachable
		    ))))
(set-impl 'nothing 'functor 'fmap
	  (lambda (x f)
	    (declare (ignore f))
	    x))

(defun fmap (functor f)
  (let ((impl (gethash 'fmap (find-impl functor 'functor))))
    (if impl
	(funcall impl functor f))))

;; From this:

(defprotocol functor f
  (fmap f fun))
(defimpl functor maybe ; superclass
  (just ; subclasses
   (fmap
    (lambda (x f)
      (match maybe x
	     ((just y)
	      (just (funcall f y)))
	     (_				; Unreachable
	      )))))
  (nothing
   (fmap
    (lambda (x f)
      (declare (ignore f))
      x))))

;; Could also expand for `records' as such:

(defstruct vec
  (vec #()))

(set-impl 'vec 'functor
	  (lambda (x f)
	    (make-vec :vec (map 'vector f (vec-vec x)))))

(defprotocol functor vec ; just the superclass
  (fmap (lambda (x f)
	  (make-vec :vec (map 'vector f (vec-vec x))))))

;; Obviously first example just adds syntactic sugar for this form.
