

(in-package :cl-dispatcher)

(defstruct %dispatcher
  hooks
  lock)

(defmacro %atomic (disp &body thing)
  `(bt:with-lock-held ((%dispatcher-lock disp))
     ,@thing))

(mapc 'export (list 

(defun make-dispatcher ()
  (let ((d (make-%dispatcher)))
    (setf (%dispatcher-hooks d ) nil)
    (setf (%dispatcher-lock d) (bt:make-lock))
    d))

(defmacro make ()
  `(make-dispatcher))

(defun hook (disp name lam)
  (%atomic disp
	   (if (assoc name (%dispatcher-hooks disp))
	     (push lam (cdr (assoc name (%dispatcher-hooks disp))))
	     (push (cons name (list lam)) (%dispatcher-hooks disp)))))

(defun sig (disp name &optional (x nil))
  (%atomic disp
	   (let ((hooks (assoc name (%dispatcher-hooks disp))))
	     (if (null hooks)
	       nil
	       (mapcar #'(lambda (y) (bt:make-thread (lambda () (funcall y x)))) (cdr hooks))))))

(defun sig-serial (disp name  &optional (x nil))
  (%atomic disp
	   (let ((hooks (assoc name (%dispatcher-hooks disp))))
	     (if (null hooks)
	       nil
	       (mapc #'(lambda (y) (funcall y x)) (cdr hooks))))))

))

(defun test ()
  (let ((d (make-dispatcher)))
    (hook d "test" (lambda (x) (print x)))
    (hook d "test" (lambda (x) (print (cons "!!" x))))
    (hook d "test" (lambda (x) (print "HELLO")))
    
    (sig-serial d "test" 'uwu)
    (print 'signalled)))

