(in-package #:cl-jupyter)

#|

# Representation and manipulation of kernel messages #

|#

#|

## Message header ## 

|#

(defclass header ()
  ((date :initarg :date :initform (fredo-utils:current-date-time) :reader header-date :type string)
   (msg-id :initarg :msg-id :reader header-msg-id :type string)
   (username :initarg :username :reader header-username :type string)
   (session :initarg :session :reader header-session :type string)
   (msg-type :initarg :msg-type :reader header-msg-type :type string)
   (version :initarg :version :initform +KERNEL-PROTOCOL-VERSION+ :reader header-version :type string))
  (:documentation "Header representation for IPython messages"))

#|

### JSon encoding ###

|#

(defmethod encode-json (stream (object header) &key (indent nil) (first-line nil))
  (with-slots (date msg-id username session msg-type version) object
    (encode-json stream `(("date" . ,date)
                          ("msg_id" . ,msg-id)
                          ("username" . ,username)
                          ("session" . ,session)
                          ("msg_type" . ,msg-type)
                          ("version" . ,version))
                 :indent indent :first-line first-line)))

(example-progn
 (defparameter *header1* (make-instance 'header
                                        :date "dummy-date"
                                        :msg-id "XXX-YYY-ZZZ-TTT"
                                        :username "fredokun"
                                        :session "AAA-BBB-CCC-DDD"
                                        :msg-type "execute_request"
                                        :version "5.1")))
(example
 (encode-json-to-string *header1* :indent 0)
 => "{
  \"date\": \"dummy-date\",
  \"msg_id\": \"XXX-YYY-ZZZ-TTT\",
  \"username\": \"fredokun\",
  \"session\": \"AAA-BBB-CCC-DDD\",
  \"msg_type\": \"execute_request\",
  \"version\": \"5.1\"
}")

(example
 (encode-json-to-string *header1*)
 => "{\"date\": \"dummy-date\",\"msg_id\": \"XXX-YYY-ZZZ-TTT\",\"username\": \"fredokun\",\"session\": \"AAA-BBB-CCC-DDD\",\"msg_type\": \"execute_request\",\"version\": \"5.1\"}")

#|

### JSon decoding ###

|#

(example (parse-json-from-string (encode-json-to-string *header1*))
         => '(("date" . "dummy-date") 
              ("msg_id" . "XXX-YYY-ZZZ-TTT") ("username" . "fredokun")
              ("session" . "AAA-BBB-CCC-DDD") ("msg_type" . "execute_request")
              ("version" . "5.1")))

(example
 (afetch "msg_id" (parse-json-from-string (encode-json-to-string *header1*)) :test #'equal)
 => "XXX-YYY-ZZZ-TTT")

(example
 (afetch "username" (parse-json-from-string (encode-json-to-string *header1*)) :test #'equal)
 => "fredokun")

#|

### Wire-deserialization ###

The deserialization of a message header from a JSon string is then trivial.

|#

(defun wire-deserialize-header (hdr)
  (let ((json-list (parse-json-from-string hdr)))
    (if json-list
        (make-instance 'header
                       :date (afetch "date" json-list :test #'equal :default (fredokun-utilities:current-date-time))
                       :msg-id (afetch "msg_id" json-list :test #'equal)
                       :username (afetch "username"json-list :test #'equal)
                       :session (afetch "session" json-list :test #'equal)
                       :msg-type (afetch "msg_type" json-list :test #'equal)
                       :version (afetch "version" json-list :test #'equal))
        nil)))

(example-progn
 (defparameter *header2* (wire-deserialize-header (encode-json-to-string *header1*))))


(example (header-username *header2*)
         => "fredokun")

#|

## IPython messages ##

|#

(defclass message ()
  ((header :initarg :header :accessor message-header)
   (parent-header :initarg :parent-header :initform nil :accessor message-parent-header)
   (metadata :initarg :metadata :initform nil :accessor message-metadata)
   (content :initarg :content :initform nil :accessor message-content)
   (buffers :type array :initarg :buffers :initform #() :accessor message-buffers))
  (:documentation "Representation of IPython messages"))

(defmethod encode-json (stream (object message) &key (indent nil) (first-line nil))
  (with-slots (header parent-header metadata content buffers) object
    (encode-json stream `(( "buffers" . ,buffers)
                          ( "content" . ,content)
                          ( "header" . ,header)
                          ( "metadata" . ,metadata)
                          ( "parent-header" . ,parent-header))
                 :indent indent :first-line first-line)))

(defun make-message (parent_msg msg_type metadata content &optional (buffers #()))
  (check-type buffers array)
  (let ((hdr (message-header parent_msg)))
    (make-instance 
     'message
     :header (make-instance 
              'header
              :date (fredo-utils:current-date-time)
              :msg-id (format nil "~W" (uuid:make-v4-uuid))
              :username (header-username hdr)
              :session (header-session hdr)
              :msg-type msg_type
              :version (header-version hdr))
     :parent-header hdr
     :metadata metadata
     :content content
     :buffers buffers)))

(defun make-orphan-message (session-id msg-type metadata content buffers)
  (check-type buffers array)
  (make-instance 
   'message
   :header (make-instance 
            'header
            :date (fredo-utils:current-date-time)
            :msg-id (format nil "~W" (uuid:make-v4-uuid))
            :username "kernel"
            :session session-id
            :msg-type msg-type
            :version +KERNEL-PROTOCOL-VERSION+)
   :parent-header '()
   :metadata metadata
   :content content
   :buffers buffers))

(defun make-custom-message (&key content (buffers #()))
  (check-type buffers array)
  (make-instance
   'message
   :header nil
   :parent-header nil
   :metadata nil
   :content content
   :buffers buffers))

(example-progn
 (defparameter *msg1* (make-instance 'message :header *header1*)))


#|

## Wire-serialization ##

The wire-serialization of IPython kernel messages uses multi-parts ZMQ messages.

|#

(defun octets-to-hex-string (bytes)
  (apply #'concatenate (cons 'string (map 'list (lambda (x) (format nil "~(~2,'0X~)" x)) bytes))))

(defun message-signing (key parts)
  #+clasp(let* ((all-parts (with-output-to-string (sout)
                             (loop for part in parts
                                   do (princ part sout))))
                (all-parts-octets (fredokun-utilities:string-to-octets all-parts)))
           (core:hmac-sha256 all-parts-octets key))
  #-clasp(let ((hmac (ironclad:make-hmac key :SHA256)))
           ;; updates
           (loop for part in parts
                 do (let ((part-bin (fredokun-utilities:string-to-octets part)))
                      (ironclad:update-hmac hmac part-bin)))
           ;; digest
           (octets-to-hex-string (ironclad:hmac-digest hmac))))

(example
 (message-signing (fredokun-utilities:string-to-octets "toto") '("titi" "tata" "tutu" "tonton"))
 => "d32d091b5aabeb59b4291a8c5d70e0c20302a8bf9f642956b6affe5a16d9e134")

;; XXX: should be a defconstant but  strings are not EQL-able...
(defvar +WIRE-IDS-MSG-DELIMITER+ "<IDS|MSG>")
(defvar +WIRE-IDS-MSG-DELIMITER-UB-VECTOR+ (fredokun-utilities:string-to-octets +WIRE-IDS-MSG-DELIMITER+))

(defmethod wire-serialize ((msg message) &key (identities nil) (key nil))
  (dolist (id identities) (check-type id (simple-array (unsigned-byte 8) (*))))
  (logg 1 ">>>>>>>>> in wire-serialize time: ~s~%" (fredokun-utilities:current-date-time))
  (with-slots (header parent-header metadata content buffers) msg
    (logg 1 "~a~%" (with-output-to-string (sout) (encode-json sout msg :indent 3 :first-line t)))
    (logg 2 "header -> ~s~%" header)
    (logg 2 "parent-header -> ~s~%" parent-header)
    (logg 2 "metadata -> ~s~%" metadata)
    (logg 2 "content -> ~s~%" content)
    (logg 2 "Number of buffers  -> ~d~%" (length buffers))
    (let ((header-json (encode-json-to-string header))
          (parent-header-json (if parent-header
                                  (encode-json-to-string parent-header)
				"{}"))
          (metadata-json (if metadata
                             (encode-json-to-string metadata)
			   "{}"))
          (content-json (if content
                            (encode-json-to-string content)
			  "{}")))
      (logg 2 "content: ~s~%" content-json)
      (logg 2 "header: ~s~%" header-json)
      (logg 2 "parent-header: ~s~%" parent-header-json)
      (logg 2 "About to calculate signature~%")
      (let ((sig (if key
                     (message-signing key (list header-json parent-header-json metadata-json content-json))
		   "")))
        (logg 2 "About to do append~%")
        (append identities
                (list* +WIRE-IDS-MSG-DELIMITER-UB-VECTOR+
		       (fredokun-utilities:string-to-octets sig)
		       (fredokun-utilities:string-to-octets header-json)
		       (fredokun-utilities:string-to-octets parent-header-json)
		       (fredokun-utilities:string-to-octets metadata-json)
		       (fredokun-utilities:string-to-octets content-json)
		       (coerce buffers 'list)))))))

(example-progn
 (defparameter *wire1*
   (wire-serialize *msg1*
		   :identities (list
				(fredokun-utilities:string-to-octets "XXX-YYY-ZZZ-TTT")
				(fredokun-utilities:string-to-octets "AAA-BBB-CCC-DDD")))))


#|

## Wire-deserialization ##

The wire-deserialization part follows.

|#

(example (position +WIRE-IDS-MSG-DELIMITER-UB-VECTOR+ *wire1* :test #'equalp)
         => 2)

(example (babel:octets-to-string (nth (position +WIRE-IDS-MSG-DELIMITER-UB-VECTOR+ *wire1* :test #'equalp) *wire1*))
         => +WIRE-IDS-MSG-DELIMITER+)

(example
 (mapcar (lambda (x) (babel:octets-to-string x))
	 (subseq *wire1* 0 (position +WIRE-IDS-MSG-DELIMITER-UB-VECTOR+ *wire1* :test #'equalp)))
 => '( "XXX-YYY-ZZZ-TTT" "AAA-BBB-CCC-DDD"))

(example
 (subseq *wire1* (+ 6 (position +WIRE-IDS-MSG-DELIMITER-UB-VECTOR+ *wire1* :test #'equalp)))
 => nil)

(example
 (let ((delim-index (position +WIRE-IDS-MSG-DELIMITER-UB-VECTOR+ *wire1* :test #'equalp)))
   (mapcar #'babel:octets-to-string (subseq *wire1* (+ 2 delim-index) (+ 6 delim-index))))
   => '("{\"date\": \"dummy-date\",\"msg_id\": \"XXX-YYY-ZZZ-TTT\",\"username\": \"fredokun\",\"session\": \"AAA-BBB-CCC-DDD\",\"msg_type\": \"execute_request\",\"version\": \"5.1\"}"
	"{}" "{}" "{}"))


(defun wire-deserialize (parts)
  (logg 1 "<<<<<< in wire-deserialize time: ~s~%" (fredokun-utilities:current-date-time))
  (logg 2 "  (length parts) -> ~d~%" (length parts))
  (let ((delim-index (position +WIRE-IDS-MSG-DELIMITER-UB-VECTOR+ parts :test  #'equalp)))
    (when (not delim-index) (error "no <IDS|MSG> delimiter found in message parts"))
    (logg 2 "     delim-index -> ~d~%" delim-index)
    (logg 2 "     parts -> ~s~%" parts)
    (let ((identities (subseq parts 0 delim-index))
          (signature (nth (1+ delim-index) parts)))
      (let ((msg (destructuring-bind (header parent-header metadata content &rest buffers)
				     (subseq parts (+ 2 delim-index) #+(or)(+ 6 delim-index)) ; the buffers are destructured
				     (make-instance 'message
						    :header (wire-deserialize-header (babel:octets-to-string header))
						    :parent-header (wire-deserialize-header (babel:octets-to-string parent-header))
						    :metadata (babel:octets-to-string metadata)
						    :content (babel:octets-to-string content)
						    :buffers (coerce buffers 'vector)))))
        (logg 1 "~a~%" (with-output-to-string (sout)
					      (encode-json sout msg :indent 0 :first-line t)))
        (values identities
                signature
                msg)))))


(example-progn
 (defparameter *dewire-1* (multiple-value-bind (ids sig msg)
                              (wire-deserialize *wire1*)
                            (list ids sig msg))))

(example
 (header-username (message-header (third *dewire-1*)))
 => "fredokun")

#|

### Sending and receiving messages ###

|#

(defun bstr (vec)
  (with-output-to-string (sout)
    (loop for x across vec
          do (cond
               ((= x #.(char-code #\"))
                (princ "\"" sout))
               ((< x 32)
                (princ "\\x" sout)
                (format sout "~2,'0x" x))
               ((>= x 128)
                (princ "\\x" sout)
                (format sout "~2,'0x" x))
               (t (write-char (code-char x) sout))))))




;; Locking, courtesy of dmeister, thanks !
(defun message-send (channel msg &key (identities nil) (key nil))
  (bordeaux-threads:with-lock-held ((send-lock channel))
    (let ((wire-parts (wire-serialize msg :identities identities :key key)))
      (logg 2 "  in message-send (length wire-parts) -> ~s~%" (length wire-parts))
      (logg 2 "    send wire-parts-> ~s~%" (loop for x in wire-parts collect (bstr x)))
      (logg 2 "    send (pzmq:getsockopt socket :type) -> ~s~%" (pzmq:getsockopt (socket channel) :type))
      (logg 2 "    send (pzmq:getsockopt socket :identity) -> ~s~%" (pzmq:getsockopt (socket channel) :identity))
      (flet ((send-part (part sndmore)
               ;; Clasp supports binary buffers using clasp-ffi:foreign-data
               (cond
                 ((typep part 'clasp-ffi:foreign-data)
                  (pzmq:send (socket channel) part :len (clasp-ffi:foreign-data-size part) :sndmore sndmore))
                 ((typep part '(array (unsigned-byte 8)))
                  (cffi:with-foreign-object (buf :uint8 (length part))
                    (dotimes (i (length part)) (setf (cffi:mem-aref buf :uint8 i) (elt part i)))
                    (logg 2 "message-send (array (unsigned-byte 8)): ~s~%"
                          (loop for x below (length part) collect (cffi:mem-aref buf :uint8 x)))
                    (logg 2 "                  AKA (as byte-string): ~s~%" (bstr part))
                    (pzmq:send (socket channel) buf :len (length part) :sndmore sndmore)))
                 (t (error "Cannot send part ~s of type ~s" part (type-of part))))
               #-clasp(pzmq:send (socket channel) part :sndmore sndmore)))
        ;; Ensure that the last part send has sndmore = NIL
        (do* ((cur wire-parts (cdr cur))
              (part (car cur) (car cur))
              (sndmore (cdr cur) (cdr cur)))
             ((null cur))
          (send-part part sndmore))))))

(defun recv-array-bytes (socket &key dontwait (encoding cffi:*default-foreign-encoding*))
  "Receive a message part from a socket as a string."
  (pzmq:with-message
    msg
    (pzmq:msg-recv msg socket :dontwait dontwait)
    (values
     (let* ((data (pzmq:msg-data msg))
	    (len (pzmq:msg-size msg))
	    (array-bytes (make-array len :element-type 'ext:byte8)))
       (loop for index from 0 below len
	     do (setf (aref array-bytes index) (cffi:mem-aref data :uint8 index)))
       array-bytes)
     (pzmq:getsockopt socket :rcvmore))))

(defun zmq-recv-list (socket &optional (parts nil) (part-num 1))
  (multiple-value-bind (part more)
      (recv-array-bytes socket)
    ;;(jformat t "[Shell]: received message part #~A: ~W (more? ~A)~%" part-num part more)
    (if more
        (zmq-recv-list socket (cons part parts) (+ part-num 1))
        (reverse (cons part parts)))))


(defun message-recv (channel)
  (bordeaux-threads:with-lock-held ((recv-lock channel))
    (let ((parts (zmq-recv-list (socket channel))))
      (logg 2 "=============== message-recv ==============~%")
      (logg 2 "    recv (pzmq:getsockopt socket :type) -> ~s~%" (pzmq:getsockopt (socket channel) :type))
      (logg 2 "    recv (pzmq:getsockopt socket :identity) -> ~s~%" (pzmq:getsockopt (socket channel) :identity))
      ;;DEBUG>>
      ;;(jformat t "[Recv]: parts: ~A~%" (mapcar (lambda (part) (format nil "~W" part)) parts))
      (wire-deserialize parts))))



