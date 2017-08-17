(in-package :cl-jupyter-widgets)

(defclass value-widget (widget)
  ((value :initarg :value :accessor value
	 :initform nil)))

(defmethod get-interact-value ((self value-widget))
  (value self))


(defmethod %repr-keys ((self value-widget))
  )

;;FIXME
#|
 def _repr_keys(self):
        # Ensure value key comes first, and is always present
        yield 'value'
        for key in super(ValueWidget, self)._repr_keys():
            if key != 'value':
                yield key
|#
