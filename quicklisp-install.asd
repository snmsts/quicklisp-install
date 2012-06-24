(asdf:defsystem #:quicklisp-install
  :serial t
  :version "0.0"
  :components ((:file "package")
	       (:file "install"))
  :depends-on (:drakma))