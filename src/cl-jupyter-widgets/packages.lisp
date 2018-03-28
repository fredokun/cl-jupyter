
(defpackage #:cl-jupyter-widgets
  (:nicknames #:cljw)
  (:use #:cl)
  (:shadow #:open #:close #:step #:min #:max)
  (:export
   #:*kernel-start-hook*
   #:*kernel-shutdown-hook*
   #:*handle-comm-open-hook*
   #:*handle-comm-msg-hook*
   #:*handle-comm-close-hook*
   #:*send-updates*
   #:notify-change
   #:ipython-display
   #:widget
   #:widget-open
   #:widget-send
   #:widget-close
   #:domwidget
   #:int-slider
   #:image
   #:bool
   #:dict
   #:unicode
   #:cunicode
   #:tuple
   #:color
   #:instance
   #:on-msg
   #:on-displayed
   #:assoc-value
   ;;;begin kevin symbols
   ;;;from widget_bool_7
   #:checkbox
   #:toggle-button
   #:valid
   ;;;from widget_int_7
   #:int-text
   #:bounded-int-text
   #:int-slider
   #:int-progress
   #:int-range-slider
   ;;;from widget_float_7
   #:float-text
   #:bounded-float-text
   #:float-slider
   #:float-progress
   #:float-range-slider
   ;;;from widget_selection_7
   #:toggle-buttons
   #:dropdown
   #:radio-buttons
   #:select
   #:select-multiple
   #:selection-slider
   #:selection-range-slider
   ;;;from widget_selectioncontainer_7
   #:accordion
   #:tab
   ;;;from widget_button_7
   #:button
   #:%handle-button-msg
   ;;;from widget_box_7
   #:vbox
   #:hbox
   ;;;from widget_color_7
   #:color-picker
   ;;;from widget_string_7
   #:html
   #:html-math
   #:label
   #:textarea
   #:text
   #:password
   ))

(defpackage #:traitlets
  (:use #:cl)
  (:export #:traitlet-class #:synced-object)
  (:export #:traitlet-metadata
	   #:effective-traitlet))
