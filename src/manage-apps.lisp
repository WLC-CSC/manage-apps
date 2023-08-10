(in-package :manage-apps)

;; server settings
(defvar *port-swank* 4005)
(defvar *port-server* 4006)
(defvar *server* nil)

;; error handling settings
(setf hunchentoot:*catch-errors-p* nil) ;; setf t in production

(defun main ()
  (swank:create-server :port *port-swank* :dont-close t)
  (setf *server* (make-instance 'hunchentoot:easy-acceptor :port *port-server*))
  (hunchentoot:start *server*))
