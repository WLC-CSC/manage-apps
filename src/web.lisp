(in-package :manage-apps)

;;; project and working directory values
(defvar *directory* (merge-pathnames "scratch/" (uiop:temporary-directory)))
(defvar *project* nil)

;;; generating json objects for server responses
(defun make-response (status messages)
  "Create a json response in the form {'status': status, 'messages': ['message']}"
  (let ((msg (cond
               ((and (stringp messages) (= (length messages) 0)) #()) ; empty string, no messages -> []
               ((stringp messages) (list messages)) ; only one string message, ensure it's a list
               ((null messages) #())
               (t messages))))
    (json:encode-json-to-string `((:status . ,status) (:messages . ,msg)))))

(defun response-success (messages)
  "Response signalling that the operation was successful"
  (make-response "success" messages))

(defun response-error (messages)
  "Response signalling that the operation failed"
  (make-response "error" messages))

(define-condition board-error (error)
  ((text :initarg :text :reader text))
  (:documentation "Condition to signal when creating or running a project fails"))

(defmacro with-response (&body body)
  "Returns success or failure responses after executing the body"
  `(handler-case
       (response-success (progn
                           ,@body))
     (board-error (c)
       (response-error (text c)))))

;;; endpoints
(hunchentoot:define-easy-handler (upload :uri "/upload") ()
  (setf (hunchentoot:content-type*) "application/json")
  (when (eq (hunchentoot:request-method hunchentoot:*request*) :post)
    (with-response
      (upload-files (hunchentoot:post-parameters*)))))


(defun upload-files (parameters)
  "Move the uploaded files into the scratch directory and create a project"
  ;; create an empty scratch directory
  (make-scratch-directory)

  ;; clear any current project
  (when *project*
    (project-cleanup *project*))

  ;; move files to scratch, create project, and build it
  (let* ((uploaded-files (move-files-to-scratch-directory parameters))
         (project (make-cpp-project *directory* uploaded-files))
         (result (project-build project)))
    (setf *project* project)
    result))

(defun make-scratch-directory ()
  "Create a scratch directory for building and running projects"
  (uiop:delete-directory-tree *directory* :validate t :if-does-not-exist :ignore)
  (ensure-directories-exist *directory*))

(defun move-files-to-scratch-directory (parameters)
  "Extract the filenames and temporary paths from parameters, then
move them to the scratch directory. Returns the paths to the moved
files"
  (loop
    for (filename path . rest) in parameters
    for new-filename = (merge-pathnames filename *directory*)
    for new-filepath = (rename-file path new-filename)
    collect new-filepath))

(hunchentoot:define-easy-handler (start :uri "/start") ()
  (setf (hunchentoot:content-type*) "application/json")
  (when (eq (hunchentoot:request-method hunchentoot:*request*) :post)
    (with-response
      (unless *project*
        (error 'board-error :text "No project"))
      (project-start *project*))))


(hunchentoot:define-easy-handler (stop :uri "/stop") ()
  (setf (hunchentoot:content-type*) "application/json")
  (when (eq (hunchentoot:request-method hunchentoot:*request*) :post)
    (with-response
      (if *project*
          (project-stop *project*)
          (error 'board-error :text "No project")))))



(hunchentoot:define-easy-handler (info :uri "/info") ()
  (setf (hunchentoot:content-type*) "application/json")
  (when (eq (hunchentoot:request-method hunchentoot:*request*) :post)
    (with-response
      (unless *project*
        (error 'board-error :text "No project"))
      (project-info *project*))))
