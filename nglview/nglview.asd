
(asdf:defsystem #:nglview
    :description "The ngl widget for cl-jupyter with widgets"
    :version "0.1"
    :author "Kevin Esslinger"
    :license "LGPL2. See LICENSE."
    :depends-on (:cl-jupyter)
    :serial t
    :components (
		 (:file "packages")
		 (:file "utils")
		 (:file "shape")
		 (:file "pythread")
		 (:file "widget")
		 (:file "base_adaptor")
		 (:file "adaptor")
		 (:file "show")
		 ))
