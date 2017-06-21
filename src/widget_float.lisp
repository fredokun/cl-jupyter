(in-package :cl-jupyter-widgets)

#||

class _Float(DOMWidget):
    value = CFloat(0.0, help="Float value").tag(sync=True)
    disabled = Bool(False, help="Enable or disable user changes").tag(sync=True)
    description = Unicode(help="Description of the value this widget represents").tag(sync=True)

    _model_module = Unicode('jupyter-js-widgets').tag(sync=True)
    _view_module = Unicode('jupyter-js-widgets').tag(sync=True)

    def __init__(self, value=None, **kwargs):
        if value is not None:
            kwargs['value'] = value
super(_Float, self).__init__(**kwargs)

||#

(defclass %float (dom-widget)
  ((value :initarg :value :accessor value
	   :type float
	   :initform 0.0
	   :metadata (:sync t
			    :json-name "value"
			    :help "Float value"))
   (disabled :initarg :disabled :accessor disabled
	      :type boolean
	      :initform :false
	      :metadata (:sync t
			       :json-name "disabled"
			       :help "enable or disable user changes"))
   (description :initarg :description :accessor description
		 :type unicode
		 :initform (unicode "")
		 :metadata (:sync t
				  :json-name "description"
				  :help "Description of the value this widget represents"))
   )
  (:default-initargs
   :model-module (unicode "jupyter-js-widgets")
    :view-module (unicode "jupyter-js-widgets")
    )
  (:metaclass traitlets:traitlet-class))

#||

class _BoundedFloat(_Float):
    max = CFloat(100.0, help="Max value").tag(sync=True)
    min = CFloat(0.0, help="Min value").tag(sync=True)
    step = CFloat(0.1, help="Minimum step to increment the value (ignored by some views)").tag(sync=True)

||#

(defclass %bounded-float(%float)
  ((max :initarg :max :accessor max
	 :type float
	 :initform 100.0
	 :metadata (:sync t
			  :json-name "max"
			  :help "Max value"))
   (min :initarg :min :accessor min
	 :type float
	 :initform 0.0
	 :metadata (:sync t
			  :json-name "min"
			  :help "Min value"))
   (step :initarg :step :accessor step
	  :type float
	  :initform 0.1
	  :metadata (:sync t
			   :json-name "step"
			   :help "Minimum step to increment the value (ignored by some views)"))
   )
  (:metaclass traitlets:traitlet-class))

#||

@register('Jupyter.FloatText')
class FloatText(_Float):
    """ Displays a float value within a textbox. For a textbox in
    which the value must be within a specific range, use BoundedFloatText.
    Parameters
    ----------
    value : float
        value displayed
    description : str
        description displayed next to the text box
    color : str Unicode color code (eg. '#C13535')
        color of the value displayed
    """
    _view_name = Unicode('FloatTextView').tag(sync=True)
_model_name = Unicode('FloatTextModel').tag(sync=True)

||#

(defclass float-text(%float)
  ()
  (:default-initargs
   :view-name (unicode "FloatTextView")
    :model-name (unicode "FloatTextModel")
    )
  (:metaclass traitlets:traitlet-class))

#||

@register('Jupyter.BoundedFloatText')
class BoundedFloatText(_BoundedFloat):
    """ Displays a float value within a textbox. Value must be within the range specified.
    For a textbox in which the value doesn't need to be within a specific range, use FloatText.
    Parameters
    ----------
    value : float
        value displayed
    min : float
        minimal value of the range of possible values displayed
    max : float
        maximal value of the range of possible values displayed
    description : str
        description displayed next to the textbox
    color : str Unicode color code (eg. '#C13535')
        color of the value displayed
    """
    _view_name = Unicode('FloatTextView').tag(sync=True)
_model_name = Unicode('FloatTextModel').tag(sync=True)

||#

(defclass bounded-float-text(%bounded-float)
  ()
  (:default-initargs
   :view-name (unicode "FloatTextView")
    :model-name (unicode "FloatTextModel"))
  (:metaclass traitlets:traitlet-class))

#||

@register('Jupyter.FloatSlider')
class FloatSlider(_BoundedFloat):
    """ Slider/trackbar of floating values with the specified range.
    Parameters
    ----------
    value : float
        position of the slider
    min : float
        minimal position of the slider
    max : float
        maximal position of the slider
    step : float
        step of the trackbar
    description : str
        name of the slider
    orientation : {'horizontal', 'vertical'}
        default is 'horizontal', orientation of the slider
    readout : {True, False}
        default is True, display the current value of the slider next to it
    readout_format : str
        default is '.2f', specifier for the format function used to represent
        slider value for human consumption, modeled after Python 3's format
        specification mini-language (PEP 3101).
    slider_color : str Unicode color code (eg. '#C13535')
        color of the slider
    color : str Unicode color code (eg. '#C13535')
        color of the value displayed (if readout == True)
    """
    _view_name = Unicode('FloatSliderView').tag(sync=True)
    _model_name = Unicode('FloatSliderModel').tag(sync=True)
    orientation = CaselessStrEnum(values=['horizontal', 'vertical'],
        default_value='horizontal', help="Vertical or horizontal.").tag(sync=True)
    _range = Bool(False, help="Display a range selector").tag(sync=True)
    readout = Bool(True, help="Display the current value of the slider next to it.").tag(sync=True)
    readout_format = Unicode('.2f', help="Format for the readout").tag(sync=True)
    slider_color = Color(None, allow_none=True).tag(sync=True)
    continuous_update = Bool(True, help="Update the value of the widget as the user is holding the slider.").tag(sync=True)

||#

(defclass float-slider(%bounded-float)
  ((orientation :initarg :orientation :accessor orientation
		 :type unicode
		 :initform "horizontal"
		 :metadata (:sync t
				  :json-name "orientation"
				  :help "Vertical or horizontal"))
   (_range :initarg :range :accessor range
	   :type boolean
	   :initform :false
	   :metadata (:sync t
			    :json-name "_range"
			    :help "Display a range selector"))
   (readout :initarg :readout :accessor readout
	     :type boolean
	     :initform :true
	     :metadata (:sync t
			      :json-name "readout"
			      :help "Display the current value of the slider next to it"))
   (readout-format :initarg :readout-format :accessor readout-format
		    :type unicode
		    :initform (unicode ".2f")
		    :metadata (:sync t
				     :json-name "readout_format"
				     :help "Format for the readout"))
   (slider-color :initarg :slider-color :accessor slider-color
		  :type unicode
		  :initform (unicode "None")
		  :metadata (:sync t
				   :json-name "slider_color"))
   (continuous-update :initarg :continuous-update :accessor continuous-update
		       :type boolean
		       :initform :true
		       :metadata (:sync t
					:json-name "continuous_update"
					:help "Update the value of the widget as the user is holding the slider."))
   )
  (:default-initargs
   :view-name (unicode "FloatSliderView")
    :model-name (unicode "FloatSliderModel"))
  (:metaclass traitlets:traitlet-class))

#||

@register('Jupyter.FloatProgress')
class FloatProgress(_BoundedFloat):
    """ Displays a progress bar.
    Parameters
    -----------
    value : float
        position within the range of the progress bar
    min : float
        minimal position of the slider
    max : float
        maximal position of the slider
    step : float
        step of the progress bar
    description : str
        name of the progress bar
    orientation : {'horizontal', 'vertical'}
        default is 'horizontal', orientation of the progress bar
    bar_style: {'success', 'info', 'warning', 'danger', ''}
        color of the progress bar, default is '' (blue)
        colors are: 'success'-green, 'info'-light blue, 'warning'-orange, 'danger'-red
    """
    _view_name = Unicode('ProgressView').tag(sync=True)
    _model_name = Unicode('ProgressModel').tag(sync=True)
    orientation = CaselessStrEnum(values=['horizontal', 'vertical'],
        default_value='horizontal', help="Vertical or horizontal.").tag(sync=True)

    bar_style = CaselessStrEnum(
        values=['success', 'info', 'warning', 'danger', ''],
        default_value='', allow_none=True, help="""Use a predefined styling for
        the progess bar.""").tag(sync=True)

||#

(defclass float-progress (%bounded-float)
  ((orientation :initarg :orientation :accessor orientation
		 :type unicode
		 :initform (unicode "horizontal")
		 :metadata (:sync t
				  :json-name "orientation"
				  :help "Vertical or horizontal."))
   (bar-style :initarg :bar-style :accessor bar-style
	       :type unicode
	       :initform (unicode "")
	       :metadata (:sync t
				:json-name "bar_style"
				:help "Use a predefined styling for the progress bar. Options: 'success', 'info', 'warning', 'danger'"))
   )
  (:default-initargs
   :view-name (unicode "ProgressView")
    :model-name (unicode "ProgressModel"))
  (:metaclass traitlets:traitlet-class))

#||

class _FloatRange(_Float):
    value = Tuple(CFloat(), CFloat(), default_value=(0.0, 1.0),
help="Tuple of (lower, upper) bounds").tag(sync=True)

||#

(defclass %float-range (%float)
  ((value :initarg :value :accessor value
	   :type tuple
	   :initform (tuple 0.0 1.0)
	   :metadata (:sync t
			    :json-name "value"
			    :help "Tuple of (lower, upper) bounds"))
   )
  (:metaclass traitlets:traitlet-class))


#||

class _BoundedFloatRange(_FloatRange):
    step = CFloat(1.0, help="Minimum step that the value can take (ignored by some views)").tag(sync=True)
    max = CFloat(100.0, help="Max value").tag(sync=True)
min = CFloat(0.0, help="Min value").tag(sync=True)

||#

(defclass %bounded-float-range(%float-range)
  ((step :initarg :step :accessor step
	  :type float
	  :initform 1.0
	  :metadata (:sync t
			   :json-name "step"
			   :help "Minimum step that the value can take (ignored by some views)"))
   (max :initarg :max :accessor max
	 :type float
	 :initform 100.0
	 :metadata (:sync t
			  :json-name "max"
			  :help "Max value"))
   (min :initarg :min :accessor min
	 :type float
	 :initform 0.0
	 :metadata (:sync t
			  :json-name "min"
			  :help "Min value"))
   )
  (:metaclass traitlets:traitlet-class))


#||

@register('Jupyter.FloatRangeSlider')
class FloatRangeSlider(_BoundedFloatRange):
    """ Slider/trackbar that represents a pair of floats bounded by minimum and maximum value.
    Parameters
    ----------
    value : float tuple
        range of the slider displayed
    min : float
        minimal position of the slider
    max : float
        maximal position of the slider
    step : float
        step of the trackbar
    description : str
        name of the slider
    orientation : {'horizontal', 'vertical'}
        default is 'horizontal'
    readout : {True, False}
        default is True, display the current value of the slider next to it
    readout_format : str
        default is '.2f', specifier for the format function used to represent
        slider value for human consumption, modeled after Python 3's format
        specification mini-language (PEP 3101).
    slider_color : str Unicode color code (eg. '#C13535')
        color of the slider
    color : str Unicode color code (eg. '#C13535')
        color of the value displayed (if readout == True)
    """
    _view_name = Unicode('FloatSliderView').tag(sync=True)
    _model_name = Unicode('FloatSliderModel').tag(sync=True)
    orientation = CaselessStrEnum(values=['horizontal', 'vertical'],
        default_value='horizontal', help="Vertical or horizontal.").tag(sync=True)
    _range = Bool(True, help="Display a range selector").tag(sync=True)
    readout = Bool(True, help="Display the current value of the slider next to it.").tag(sync=True)
    slider_color = Color(None, allow_none=True).tag(sync=True)
continuous_update = Bool(True, help="Update the value of the widget as the user is sliding the slider.").tag(sync=True)

||#

(defclass float-range-slider(%bounded-float-range)
  ((orientation :initarg :orientation :accessor orientation
		 :type unicode
		 :initform (unicode "horizontal")
		 :metadata (:sync t
				  :json-name "orientation"
				  :help "Vertical or horizontal"))
   (_range :initarg :range :accessor range
	   :type boolean
	   :initform :true
	   :metadata (:sync t
			    :json-name "_range"
			    :help "Display a range selector"))
   (readout :initarg :readout :accessor readout
	     :type boolean
	     :initform :true
	     :metadata (:sync t
			      :json-name "readout"
			      :help "Display the current value of the slider next to it"))
   (readout-format :initarg :readout-format :accessor readout-format
		    :type unicode
		    :initform (unicode ".2f")
		    :metadata (:sync t
				     :json-name "readout_format"
				     :help "Format for the readout"))
   (slider-color :initarg :slider-color :accessor slider-color
		  :type unicode
		  :initform (unicode "None")
		  :metadata (:sync t
				   :json-name "slider_color"))
   (continuous-update :initarg :continuous-update :accessor continuous-update
		       :type boolean
		       :initform :true
		       :metadata (:sync t
					:json-name "continuous_update"
				        :help "Update the value of the widget as the user is sliding the slider"))
   )
  (:default-initargs
   :view-name (unicode "FloatSliderView")
    :model-name (unicode "FloatSliderModel"))
  (:metaclass traitlets:traitlet-class))

