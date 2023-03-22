(in-package :manage-apps)


(defclass project ()
  ((working-directory
    :reader project-working-directory
    :initarg :working-directory
    :initform (error "Must specify the project working directory")
    :documentation "The working directory of the project")
   (files
    :reader project-files
    :initarg :files
    :initform (error "Must specify the project files")
    :type list
    :documentation "List of files needed to build/execute project")
   (executable
    :accessor project-executable
    :initform nil
    :documentation "Name of the executable")
   (process
    :accessor project-process
    :initform nil
    :documentation "Process associate with running executable"))
  (:documentation "Base class for all projects"))


(defclass cpp-project (project)
  ()
  (:documentation "Class for building and executing C++ projects"))

(defmethod initialize-instance :after ((project cpp-project) &key)
  "Verify that the C++ project is correctly configured"
  (with-accessors ((files project-files)) project
    (unless (and (> (length files) 0)
                 (some (lambda (path) (string= "cpp" (pathname-type path))) files))
      (let ((msg "C++ projects must contain a least one cpp file, received these files instead ~a")
            (filenames (mapcar #'file-namestring files)))
        (error 'board-error :text (format nil msg filenames))))))

(defun make-cpp-project (working-directory files)
  "Create a C++ project from the given files"
  (make-instance 'cpp-project :working-directory working-directory :files files))


(defgeneric project-build (project)
  (:documentation "Build the program into an executable"))

(defmethod project-build ((project cpp-project))
  "Build the C++ project"
  (with-accessors ((wd project-working-directory) (files project-files)) project
    (let ((executable-filename (namestring (merge-pathnames "main.out" wd)))
          (compile-flags '("-D BOARD"))
          (linking-flags '("-L/home/board/rpi-rgb-led-matrix/lib" "-lrgbmatrix" "-lpthread"))
          (include-flags '("-I/home/board/rpi-rgb-led-matrix/include")))
      (let* ((filenames (mapcar #'namestring files))
             (cpp-files (remove-if-not (lambda (path) (string= "cpp" (pathname-type path))) filenames)))
        (multiple-value-bind (output error-message exit-code)
            (uiop:run-program `("g++" ,@compile-flags ,@linking-flags ,@include-flags ,@cpp-files "-o" ,executable-filename)
                              :ignore-error-status t :output :string :error-output :string)
          
          (if (= exit-code 0)
              (progn
                (setf (project-executable project) executable-filename)
                (concatenate 'string output error-message))
              (error 'board-error :text error-message)))))))


(defun clear-project ()
  "Close any open streams of *process* and set it to nil"
  (when *project*
    (with-accessors ((process project-process)) *project*
      (uiop:terminate-process process)
      (uiop:close-streams process))
    (setf *project* nil)))

(defgeneric project-start (project)
  (:documentation "Start running the project the executable"))
  
(defmethod project-start ((project cpp-project))
  "Run the C++ executable"
  (with-accessors ((executable project-executable) (process project-process)) project
    (unless executable
      (error 'board-error :text "No project executable exists"))
    (when process
      (if (uiop:process-alive-p process)
          (error 'board-error :text "Project is already running")
          (uiop:close-streams process)))
    (setf executable (namestring (merge-pathnames "main.out" *directory*)))
    (setf process (uiop:launch-program `("sudo" ,executable) :output :stream :error-output :stream))
    nil))

(defgeneric project-stop (project)
  (:documentation "Stop running the executable"))

(defmethod project-stop ((project cpp-project))
  "Stop the C++ project by terminating the process"
  (with-accessors ((process project-process)) project
    (unless process
      (error 'board-error :text "Project has no running process"))
    (unless (uiop:process-alive-p process)
      (error 'board-error :text "No running process"))
    (unless (uiop:terminate-process process)
      (error 'board-error :text "Could not stop running project"))))


(defgeneric project-cleanup (project)
  (:documentation "Remove the current project if it exists"))

(defmethod project-cleanup ((project cpp-project))
  "Terminate any running processes and remove files for the given project"
  (when *project*
    (with-accessors ((process project-process)) *project*
      (when process
        (uiop:terminate-process process)
        (uiop:close-streams process)))
    (setf *project* nil)))

(defgeneric project-info (project)
  (:documentation "Retrieve info about the project"))

(defmethod project-info ((project cpp-project))
  "Retrive info for the current C++ project"
  (with-accessors ((process project-process)) project
    (let ((running (if (and process (uiop:process-alive-p process))
                       "Program is running"
                       "Program is not running"))
          (output (when process
                    (read-from-stream (uiop:process-info-output process))))
          (errors (when process
                    (read-from-stream (uiop:process-info-error-output process)))))
      `(("running" . ,running) ("output" . ,output) ("errors" . ,errors)))))

(defun read-from-stream (stream)
  (loop while (listen stream)
    collect (read-line stream)))
