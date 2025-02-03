(asdf:defsystem :sleepsaver
    :description "Sleepsaver strategy editor"
    :version "0.0.1"
    :author "Arthur Amalvy"
    :depends-on (:clog
		 :cl-svg
		 :alexandria
		 :trivial-clipboard)
    :serial t
    :components ((:file "package")
		 (:file "utils")
		 (:file "clog-utils")
		 (:file "colors")
		 (:file "geometry")
		 (:file "svgrender")
		 (:file "state")
		 (:file "move")
		 (:file "maps")
		 (:file "configs")
		 (:file "sleepsaver"))
    :build-operation "program-op"
    :build-pathname "sleepsaver"
    :entry-point "sleepsaver:run-sleepsaver-forever")
