(in-package :cljw)

(defclass mark (widget)
  ((mark-types :initarg :mark-types :accessor mark-types
               :initform nil)
   (scales :initarg :scales :accessor scales
           :initform nil
           :validator %validate-scales
           :metadata (:sync t
                            :json-name "scales"
                            *widget-serialization*))
   (scales-metadata :initarg :scales-metadata :accessor scales-metadata
                    :initform nil
                    :metadata (:sync t
                                     :json-name "scales_metadata"))
   (preserve-domain :initarg :preserve-domain :accessor preserve-domain
                    :initform nil
                    :metadata (:sync t
                                     :json-name "preserve_domain"))
   (display-legend :initarg :display-legend :accessor display-legend
                   :type bool
                   :initform :false
                   :metadata (:sync t
                                    :json-name "preserve_domain"))
   (%labels :initarg :labels :accessor %labels
            :type list
            :initform nil
            :metadata (:sync t
                             :json-name "labels"
                             :display-name "Display legend"))
   ;;;ADD REMAINING SLOTS
   ))

(defmethod %get-dimension-scales ((self mark) dimension &key (preserve-domain nil))
  (let ((ret nil))
    (if preserve-domain
        (loop for (k . v) in (scales self)
           do
             (when (and
                    (and
                     (assoc k (scales-metadata self) :test #'string=)
                     (= (getf (assoc k (scales-metadata self) :test #'string=) ':dimension) dimension))
                    (not (getf (preserve-domain self) k)))
               (push k ret)))
        (loop for (k . v) in (scales self)
           do
             (when (and
                    (assoc k (scales-metadata self) :test #'string=)
                    (= (getf (assoc k (scales-metadata self) :test #'string=) ':dimension) dimension))
               (push k ret))))
    ret))

;;;@validate('scales')
(defmethod %validate-scales (object val)
  (let ((scales val))
    (warn "Scales validator doesn't actually validate anything"))
  val)
