(asdf:defsystem #:passwordmanagerapp
  :description "A simple password manager written in Common Lisp"
  :author "Enzo Bissoli"
  :version "0.0.1"
  :serial t
  :depends-on (#:clog #:ironclad #:lparallel #:com.inuoe.jzon #:str)
  :components ((:file "passwordmanager" "passwordmanagerguiclog")));
