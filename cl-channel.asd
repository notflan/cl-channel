;;;; cl-channel.asd

(asdf:defsystem #:cl-channel
  :description "golang-like channels for CL"
  :author "Avril <flanchan@cumallover.me>"
  :license  "None"
  :version "0.0.1"
  :serial t
  :depends-on ( :flan-utils
		:bt-semaphore )
  :components ((:file "package")
               (:file "cl-channel")
	       (:file "cl-box")
	       (:file "cl-dispatcher")))
