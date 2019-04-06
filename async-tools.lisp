
(in-package #:async-tools)

(defun val (v) v)

(defun sexpr-reader (stream char &key (func 'val))
  "Read next token only if S expression, else return as is"
  (if (char= (peek-char t stream t nil t) #\()
    (values (funcall func (read stream t nil t)) t)
    (let ((*readtable* (copy-readtable)))
      (set-macro-character char nil)
      (values (read-from-string (concatenate 'string (string char) (write-to-string (read stream t nil t)))) nil))))

(defun async-reader (stream char)
  (multiple-value-bind (thing okay) (sexpr-reader stream char)
    (if okay
      (cons 'async (list thing))
      thing)))

(defstruct promise
  thread
  value
  ended)

(defmacro async (&body form)
  "Run body in seperate thread, returns promise."
  `(let ((end (make-promise)))
     (setf (promise-value end) nil)
     (setf (promise-ended end) nil)
     (setf (promise-thread end)
	   (bt:make-thread
	     #'(lambda ()
		 (setf (promise-value end) (progn ,@form))
		 (setf (promise-ended end) t))))
     end))

(defun wait (promise)
  "Wait for promise to complete, returns the last value evaluated and T if the thread exited cleanly"
  (bt:join-thread (promise-thread promise))
  (value promise))

(defun alive? (promise)
  "Is the thread alive?"
  (bt:thread-alive-p (promise-thread promise)))

(defun value (promise)
  "The current value of the promise and T if the thread exited cleanly, (returns nil nil if it has not terminated yet)"
  (values (promise-value promise) (clean? promise)))

(defun ended? (promise)
  "Has the promise completed?"
  (promise-ended promise))

(defun thread (promise)
  "The thread for the promise"
  (promise-thread promise))

(defun clean? (promise)
  "Did the thread exit, and did it do so cleanly?"
  (and (ended? promise) (not (alive? promise))))

(defun kill (promise)
  "Kill thread. Unclean exit."
  (bt:destroy-thread (promise-thread promise)))

(mapc 'export '(
	promise
	promise-p
	wait
	alive?
	thread
	value
	ended?
	clean?
	async))

(defmacro enable-reader ()
  "Turn on reader macroi $(form) to run (async form)"
  '(eval-when (:compile-toplevel :load-toplevel :execute)
     (set-macro-character #\$ 'async-reader)))

(export 'enable-reader)
