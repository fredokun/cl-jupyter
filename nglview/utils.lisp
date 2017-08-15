(in-package :nglv)

(cljw:widget-log "nglview  Loading utils.lisp~%")



(defun get-name (obj &key dictargs (name nil name-p) &allow-other-keys)
  (if name-p
      name
      (if (dict-entry "name" dictargs)
	  (dict-value "name" dictargs)
	  (let ((raw-name (format nil "~a" obj)))
	    raw-name))))
#||
def get_name(obj, kwargs):
    name = kwargs.pop('name', str(obj))
    if name.startswith('<nglview.'):
        name = name.split()[0].strip('<')
    return name
    
||#



(defun my-split (string &key (delimiterp #'delimiterp))
  (loop :for beg = (position-if-not delimiterp string)
    :then (position-if-not delimiterp string :start (1+ end))
    :for end = (and beg (position-if delimiterp string :start beg))
    :when beg :collect (subseq string beg end)
     :while end))

(defun camelize (arg)
  (let ((snake (etypecase arg
		 (string arg)
		 (symbol (string-downcase (string arg))))))
    (let ((split-snake (my-split snake :delimiterp (lambda (c) (or (char= c #\_) (char= c #\-))))))
      (with-output-to-string (sout)
	(princ (car split-snake) sout)
	(loop for part in (cdr split-snake)
	   do (princ (string-capitalize part) sout))))))

(defun dict-entry (key dict)
  "Lookup the key in the a-list with string keys and return the entry"
  (assoc key dict :test #'equal))

(defun dict-lookup (key dict &optional (default nil default-p))
  (let ((entry (dict-entry key dict)))
    (if entry
	(cdr entry)
	(if default-p
	    default
	    (error "Could not find key ~s in ~s" key dict)))))

(defun dict-set-or-push (key dict value)
  (let ((entry (dict-entry key dict)))
    (if entry
	(progn
	  (rplacd entry value)
	  dict)
	(cons (cons key value) dict))))

(defun camelize-dict (alist)
  (mapcar (lambda (x) (cons (camelize (car x)) (cdr x))) alist))

(defun dict-from-plist (plist &key remove)
  "Convert a plist (keyword value pairs from &key arguments) to a JSON dict"
  (loop for (key value) on plist by #'cddr
     unless (member key remove)
     collect (cons (camelize key) value)))

(defun seq-to-string (seq)
  "e.g. convert [1, 3, 5] to \"@1,3,5\""
  (cond
    ((stringp seq)
     seq)
    ((vectorp seq)
      (with-output-to-string (sout)
	(princ #\@ sout)
	(loop for x across seq
	   for sep = "" then ","
	   do (princ sep sout)
	   do (princ x sout))))
    (t (error "Handle seq-to-string for ~a" seq))))

(defun get-positive-index (index size)
  (when (< index 0)
    (incf index size)
    (when (< index 0)
      (error "Index is out of range")))
  (when (>= index size)
    (error "index is out of range"))
  index)

(defun click (button)
  (%handle-button-msg button nil (list (cons "event" "click")) nil))

(defun submit (widget-text)
  (%handle-string-msg widget-text nil (list (cons "event" "submit")) nil))

(defun %update-url (fun)
  (warn "is updating the URL important?"))

#|
def _update_url(func):
    from nglview.default import NGL_BASE_URL
    func.__doc__ = func.__doc__.format(ngl_url=NGL_BASE_URL)
    return func

def encode_base64(arr, dtype='f4'):
    arr = arr.astype(dtype)
    return base64.b64encode(arr.data).decode('utf8')

def decode_base64(data, shape, dtype='f4'):
    import numpy as np
    decoded_str = base64.b64decode(data)
    return np.frombuffer(decoded_str, dtype=dtype).reshape(shape)

def display_gif(fn):
    from IPython import display
    return display.HTML('<img src="{}">'.format(fn))

@contextmanager
def tempfolder():
  """run everything in temp folder
  """
  my_temp = tempfile.mkdtemp()
  cwd = os.getcwd()
  os.chdir(my_temp)
  yield
  os.chdir(cwd)
  rmtree(my_temp)


def get_repr_names_from_dict(repr_dict, component):
    """
    
    Parameters
    ----------
    """

    try:
        this_repr_dict = repr_dict['c' + str(component)]
        return [this_repr_dict[str(key)]['name'] for key in sorted(this_repr_dict.keys())]
    except KeyError:
        return []


def get_colors_from_b64(b64_image):
    """

    Examples
    --------
    >>> view.render_image()
    >>> get_colors_from_b64(view._image_data)

    Returns
    -------
    list of tuple 
    """
    # should install PIL
    # py3

    from PIL import Image
    import io, base64

    fp = io.BytesIO(base64.b64decode(b64_image))
    image = Image.open(fp)

    return image.getcolors(int(1E6))
|#

(defclass file-manager ()
  ((%src :initarg :src :accessor src)
   (%cwd :initarg :cwd :accessor cwd)
   (%compressed :initarg :compressed :accessor compressed)
   (%ext :initarg :ext :accessor ext)
   (%unzip-backend :initarg :unzip-backend :accessor unzip-backend
		   :initform :a-dictionary-maps-extensions-to-backends))
  (:documentation   "FileManager is for internal use.

    If file is in the current folder or subfoler, use filename
    If not, open content

    Parameters
    ----------
    src : str or file-like object
        filename
    compressed : None or bool, default None
        user can specify if the given file is compressed or not.
        if None, FileManager will detect based on file extension
    "))

#||
    def read(self, force_buffer=False):
        """prepare content to send to NGL
        """
        if self.use_filename and not force_buffer:
            return self.src
        else:
            if self.compressed_ext:
                return self.unzip_backend[self.compressed_ext].open(
                    self.src).read()
            elif hasattr(self.src, 'read'):
                return self.src.read()
            else:
                if self.is_filename:
                    return open(self.src, 'rb').read()
                else:
                    return self.src

    @property
    def is_compressed(self):
        '''naive detection
        '''
        if self._compressed is None:
            if self.is_filename or self.is_url:
                return (self.src.endswith('gz') or self.src.endswith('zip') or
                        self.src.endswith('bz2'))
            else:
                return False
        else:
            return self._compressed

    @property
    def compressed_ext(self):
        if self.is_compressed and self.is_filename:
            return self.src.split('.')[-1]
        else:
            return ''

    @property
    def use_filename(self):
        if hasattr(self.src, 'read'):
            return False
        else:
            if self.is_filename:
                cwd = os.getcwd()
                root_path = os.path.dirname(os.path.abspath(self.src))
                return (cwd in root_path)
            return False

    @property
    def ext(self):
        if self._ext is not None:
            return self._ext
        else:
            if hasattr(self.src, 'read') or (not self.is_filename and
                                             not self.is_url):
                raise ValueError(
                    "you must provide file extension if using file-like object or text content"
                )
            if self.is_compressed:
                return self.src.split('.')[-2]
            else:
                return self.src.split('.')[-1]

    @property
    def is_filename(self):
        if hasattr(self.src, 'read'):
            return False
        else:
            return os.path.isfile(self.src)

    @property
    def is_binary(self):
        binary_exts = ["mmtf", "dcd", "mrc", "ccp4", "map", "dxbin"]
        return self.ext.lower() in binary_exts
||#
(defmethod is-url ((self file-manager))
  (and (stringp (src self))
       (or (string= (subseq (src self) 0 4) "http")
	   (string= (subseq (src self) 0 7) "rcsb://"))))

