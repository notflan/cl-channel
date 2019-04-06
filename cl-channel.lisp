

(in-package :cl-channel)


(defstruct %queue
  internal
  mutex)


(mapc 'export (list
(defun make-queue (&optional (from nil))
  (let ((q (make-%queue)))
    (setf (%queue-internal q) from)
    (setf (%queue-mutex q) (bt:make-lock))
    q))

(defun queue-> (q i)
  (bt:with-lock-held ((%queue-mutex q))
    (setf (%queue-internal q) (reverse (cons i (%queue-internal q))))))

(defun queue-poll (q)
  (bt:with-lock-held ((%queue-mutex q))
    (list-length (%queue-internal q))))

(defun queue<- (q)
  (bt:with-lock-held ((%queue-mutex q))
    (if (< (list-length (%queue-internal q)) 1) (values nil nil)
      (values (pop (%queue-internal q)) t))))

(defun queue-clear (q)
  (bt:with-lock-held ((%queue-mutex q))
    (setf (%queue-internal q) nil)))
))

(defstruct %channel
  internal
  mutex
  rel-send
  rel-recv
  rel-close
  max
  closed)

(defmacro %atomic (chan &body body)
  `(bt:with-lock-held ((%channel-mutex ,chan))
     ,@body))

(defun sigall (sem)
  (bt-sem:signal-semaphore sem (bt-sem:semaphore-waiters sem)))

(mapc 'export (list

(defun make-channel (&optional (max 0))
  (let ((c (make-%channel)))
    (setf (%channel-internal c) (make-queue))
    (setf (%channel-mutex c) (bt:make-lock))
    (setf (%channel-rel-send c) (bt-sem:make-semaphore))
    (setf (%channel-rel-recv c) (bt-sem:make-semaphore))
    (setf (%channel-rel-close c) (bt-sem:make-semaphore))
    (setf (%channel-max c) max)
    (setf (%channel-closed c) nil)
    c))

(defun closed (chan) (%atomic chan
  (%channel-closed chan)))

(defun poll (chan)
  (%atomic chan
	   (queue-poll (%channel-internal chan))))

(defun <- (chan)
  (let ((out nil)
	(rout nil))
    (loop while (and (null out) (not (closed chan))) do
	  (progn
	    (if (> (poll chan) 0)
	      (%atomic chan
		(when (> (queue-poll (%channel-internal chan)) 0)
		  (setf out t)
		  (setf rout (queue<- (%channel-internal chan)))
 		  (bt-sem:signal-semaphore (%channel-rel-send chan) 1)))
	      (bt-sem:wait-on-semaphore (%channel-rel-recv chan)))))
    (if (closed chan) (values nil nil)  (values rout t))))

(defun -> (chan item)
  (loop while (and (not (closed chan)) (> (%channel-max chan) 0) (%atomic chan (>= (queue-poll (%channel-internal chan)) (%channel-max chan))))
	do (bt-sem:wait-on-semaphore (%channel-rel-send)))
  (let ((lv (%atomic chan
	   (if (not (if (or (%channel-closed chan) (and (> (%channel-max chan) 0) (>= (queue-poll (%channel-internal chan)) (%channel-max chan))))
	     nil
	     (progn
	       (queue-> (%channel-internal chan) item)
	       (bt-sem:signal-semaphore (%channel-rel-recv chan) 1)
	       t)))
	     (if (%channel-closed chan) nil 'reset)
	     t))))
    (if (eq lv 'reset)
      (-> chan item)
      lv)))

(defun release (chan)
  (%atomic chan
	   (setf (%channel-closed chan) t)
	   (sigall (%channel-rel-recv chan))
	   (sigall (%channel-rel-send chan))))

(defun poll (chan)
  (%atomic chan
	   (queue-poll (%channel-internal chan))))

(defmacro make ()
  `(make-channel))

))

#|(defun test ()
  (let ((chan (make-channel)))
    $(progn
       (loop while ¬(closed chan) do (let ((val (<- chan)))
	 (pprint val)
	 (pprint ".")
	 (when (string-equal val "CLOSE")
	   (release chan))))
       (print "Thread end")
       (print "."))
     $(loop while ¬(closed chan) do
	   (progn
	     (sleep 2)
	     (-> chan 'teste)))
    (loop while ¬(closed chan) do (-> chan (write-to-string (read)))))
  (print "End"))|#
