
(asdf:defsystem #:cl-jupyter
    :description "An Enhanced Interactive Shell for Common Lisp (based on the Jupyter protocol)."
    :version "0.6"
    :author "Frederic Peschanski (format nil \"<frederic~Apeschanski~Awork~Agmail~Acom>\" \".\" \".\" \"@\" \".\")" 
    :license "BSD 2-Clause. See LICENSE."
    :depends-on (:pzmq
                 :bordeaux-threads
                 :uuid
                 :babel
                 :ironclad
                 :cl-base64)
    :serial t
;;; cl-jupyter-widgets stuff
    :components ((:file "packages")
                 (:module cl-jupyter-widgets
                          :pathname "cl-jupyter-widgets"
                          :serial t
                          :components ((:file "packages")
                                       (:file "tools")
                                       (:module ikernel
                                                :pathname "ikernel"
                                                :serial t
                                                :components ((:file "manager")
                                                             (:file "comm")))
                                       (:module iwidgets
                                                :pathname "iwidgets"
                                                :serial t
                                                :components
                                                ((:file "init")
                                                 (:file "version")
                                                 (:module widgets
                                                          :pathname "widgets"
                                                          :serial t
                                                          :components ( ;;(:file "widgets-version")
                                                                       (:file "interface")
                                                                       (:file "traitlets")
                                                                       (:file "trait_types")
                                                                       (:file "widget")
                                                                       (:file "valuewidget")
                                                                       (:file "domwidget")
                                                                       (:file "widget_style_7")
                                                                       (:file "widget_core_7")
                                                                       (:file "widget_description_7")
                                                                       (:file "widget_layout_7")
                                                                       (:file "widget_int_7")
                                                                       (:file "widget_bool_7")
                                                                       (:file "widget_color_7")
                                                                       (:file "widget_image_7")
                                                                       (:file "widget_selection_7")
                                                                       (:file "widget_float_7")
                                                                       (:file "widget_button_7")
                                                                       (:file "widget_string_7")
                                                                       (:file "widget_box_7")
                                                                       (:file "widget_selectioncontainer_7")))))))
;;; cl-jupyter stuff
                 (:file "utils")
                 (:file "myjson")
                 (:file "config")
                 (:file "message")
                 (:file "shell")
                 (:file "iopub")
                 (:file "display")
                 (:file "evaluator")
                 (:file "user")
                 (:file "kernel")
                 ))
