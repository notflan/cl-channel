;;Thing for atomising operations

(in-package :cl-box)

(flan-utils:enable-all-readers)

(defstruct %box
  value
  lock)

(defmacro %atomic (box &body re)
  `(bt:with-lock-held ((%box-lock ,box))
		     ,@re))
[
(defun make-box (&optional value)
  (let ((b (make-%box)))
    (setf (%box-value b) value)
    (setf (%box-lock b) (bt:make-lock))
    b))

(defun make (&optional value)
  (make-box value))

(defun -> (box val)
  (%atomic box
    (setf (%box-value box) val)))

(defun <- (box)
  (%atomic box
    (%box-value box)))

(defun <-! (box)
  (%box-value box))

(defun ->! (box val)
  (setf (%box-value box) val))

(defun box-lock (box)
  (%box-lock box))

(defsetf <- ->)
(defsetf @ ->)

(defun @ (box)
  (<- box))

(defmacro --> (box &body things)
  (let ((name (gensym)))
    `(let ((,name ,box))
      (bt:with-lock-held ((box-lock ,name))
  	(->! ,name (progn ,@things))
	(<-! ,name)))))

(defmacro <-- (box &body things)
  (let ((name (gensym)))
    `(let ((,name ,box))
       (bt:with-lock-held ((box-lock ,name))
	 (progn ,@things)
	 (<-! ,name)))))
	

]
