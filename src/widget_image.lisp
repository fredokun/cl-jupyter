(in-package :cl-jupyter-widgets)

#||
@register('Jupyter.Image')
class Image(DOMWidget):
    """Displays an image as a widget.
    The `value` of this widget accepts a byte string.  The byte string is the
    raw image data that you want the browser to display.  You can explicitly
    define the format of the byte string using the `format` trait (which
    defaults to "png").
    """
    _view_name = Unicode('ImageView').tag(sync=True)
    _model_name = Unicode('ImageModel').tag(sync=True)
    _model_module = Unicode('jupyter-js-widgets').tag(sync=True)
    _view_module = Unicode('jupyter-js-widgets').tag(sync=True)

    # Define the custom state properties to sync with the front-end
    image-format = Unicode('png').tag(sync=True)
    width = CUnicode().tag(sync=True)
    height = CUnicode().tag(sync=True)
    _b64value = Unicode().tag(sync=True)
||#

(defun read-file-into-byte-vector (pathname)
  (with-open-file (s pathname :element-type '(unsigned-byte 8))
    (let ((data (make-array (file-length s) :element-type '(unsigned-byte 8))))
      (read-sequence data s)
      data)))

;this is not finished yet
;need an around method for value
;quickload is not working
;need to add/compile cl-base64 into docker image


(defclass image (dom-widget)
  ((image-format :initarg :format :accessor image-format
	    :type unicode
	    :initform (unicode "png")
	    :metadata (:sync t
			     :json-name "format"))
   (width :initarg :width :accessor width
	   :type 'cunicode 
	   :initform (unicode "")
	   :metadata (:sync t
			    :json-name "width"))
         
   (height :initarg :height :accessor height
	    :type 'cunicode 
	    :initform (unicode "")
	    :metadata (:sync t
			     :json-name "height"))

   (_b64value :initarg :b64value :accessor b64value
	      :type unicode
	      :initform (unicode "")
	      :metadata (:sync t
			       :json-name "_b64value"))
   (value :initarg :value :accessor value 
	   :type bytes 
	   :initform (bytes "")))

   (:default-initargs  
       :view-name (unicode "ImageView")
     :model-name (unicode "ImageModel")
     :model-module (unicode "jupyter-js-widgets")
     :view-module (unicode "jupyter-js-widgets"))
   (:metaclass traitlets:traitlet-class))


 #||value = Bytes()

    @observe('value')
    def _value_changed(self, change):
        self._b64value = base64.b64encode(change['new'])
 ||#
  


