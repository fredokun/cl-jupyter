(in-package :nglv)

;;; Subclass cl-jupyter-widgets to add %ngl-name

(defclass button (cl-jupyter-widgets:button)
  ((%ngl-name :initform nil :initarg :%ngl-name :accessor %ngl-name))
  (:metaclass traitlets:traitlet-class))

(defclass text (cl-jupyter-widgets:text)
  ((%ngl-name :initform nil :initarg :%ngl-name :accessor %ngl-name))
  (:metaclass traitlets:traitlet-class))

(defclass int-slider (cl-jupyter-widgets:int-slider) 
  ((%ngl-name :initform nil :initarg :%ngl-name :accessor %ngl-name))
  (:metaclass traitlets:traitlet-class))

(defclass dropdown (cl-jupyter-widgets:dropdown) 
  ((%ngl-name :initform nil :initarg :%ngl-name :accessor %ngl-name))
  (:metaclass traitlets:traitlet-class))

(defclass tab (cl-jupyter-widgets:tab) 
  ((%ngl-name :initform nil :initarg :%ngl-name :accessor %ngl-name))
  (:metaclass traitlets:traitlet-class))

(defclass checkbox (cl-jupyter-widgets::checkbox) 
  ((%ngl-name :initform nil :initarg :%ngl-name :accessor %ngl-name))
  (:metaclass traitlets:traitlet-class))

(defclass float-text (cl-jupyter-widgets::float-text) 
  ((%ngl-name :initform nil :initarg :%ngl-name :accessor %ngl-name))
  (:metaclass traitlets:traitlet-class))

(defclass int-text (cl-jupyter-widgets::int-text) 
  ((%ngl-name :initform nil :initarg :%ngl-name :accessor %ngl-name))
  (:metaclass traitlets:traitlet-class))

(defclass toggle-button (cl-jupyter-widgets:toggle-button) 
  ((%ngl-name :initform nil :initarg :%ngl-name :accessor %ngl-name))
  (:metaclass traitlets:traitlet-class))

(defclass color-picker (cl-jupyter-widgets:color-picker) 
  ((%ngl-name :initform nil :initarg :%ngl-name :accessor %ngl-name))
  (:metaclass traitlets:traitlet-class))


(defclass box (cl-jupyter-widgets:box) 
  ((%ngl-name :initform nil :initarg :%ngl-name :accessor %ngl-name))
  (:metaclass traitlets:traitlet-class))

(defclass vbox (cl-jupyter-widgets:vbox) 
  ((%ngl-name :initform nil :initarg :%ngl-name :accessor %ngl-name))
  (:metaclass traitlets:traitlet-class))

(defclass hbox (cl-jupyter-widgets:hbox) 
  ((%ngl-name :initform nil :initarg :%ngl-name :accessor %ngl-name))
  (:metaclass traitlets:traitlet-class))

