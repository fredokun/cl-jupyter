(in-package :cljw)

(defclass scale (widget)
  ((scale-types :accessor scale-types
                :initform nil ;;;TODO: Fill in class names
                )
   (precedence :accessor precedence
               :initform 1
               :type integer)
   (domain-class :accessor domain-class
                 :type float
                 :initform 0)
   (reverse :accessor reverse
            :type bool
            :initform :null
            :metadata (:sync t
                             :json-name "reverse"))
   (%ipython-display :accessor ipython-display
                     :initform nil))
  (:default-initargs
   :view-name (unicode "Scale")
    :model-name (unicode "ScaleModel")
    :view-module (unicode "bqplot")
    :model-module (unicode "bqplot")
    ;;TODO: Fix view-module-version and model-module-version
    )
  (:metaclass traitlets:traitlet-class))

(defclass geo-scale (scale)
  ()
  (:default-initargs
   :view-name (unicode "GeoScale")
    :model-name (unicode "GeoScaleModel"))
  (:metaclass traitlets:traitlet-class))
                                  
    
