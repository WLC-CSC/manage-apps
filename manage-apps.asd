(asdf:defsystem "manage-apps"
  :description "Service for uploading programs to the CS board"
  :version "0.1"
  :author "Jeffrey M. Brown"
  :license "GPL3"
  :serial t
  :depends-on ("hunchentoot" "swank" "cl-json")
  :components ((:module "src"
                :components ((:file "package")
                             (:file "project")
                             (:file "web")
                             (:file "manage-apps")))))
