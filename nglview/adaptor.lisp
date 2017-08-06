(in-package :nglv)

(defclass register-backend ()
  ((:package-name :initarg package_name :accessor package_name)))

(defmethod __call__ ((self register-backend) cls)
  (setf (gethash (package-name self) *BACKENDS*) cls)
  cls)

(defclass file-structure(Structure)
  ((path :initarg :path :accessor path :initform nil)
   (fm :accessor fm :initform nil)
   (ext :accessor ext :initform nil)
   (params :accessor params :type list ;plist
	   :initform ())))

(defmethod initialize-instance :after ((file-structure file-structure) &key)
  (with-slots (path fm ext) file-structure
    (let ((pathname (pathname path)))
      (unless ext
	(setf ext (pathname-type pathname))))))

;;(defgeneric get-structure-string (Structure)
;;  (:documentation "I think this works"))
(defmethod get-structure-string ((self file-structure))
  (with-open-file (stream (path self) :direction :input)
    (let* ((entire-file (make-string (+ (file-length stream) 2)
				     :initial-element #\newline)))
      (read-sequence entire-file stream)
      entire-file)))
  #|
    def get-structure-string(self):
        return self.fm.read(force-buffer=True)
|#


(defclass cando-structure (structure)
  ((%matter :initarg :matter :accessor matter)))

(defmethod ext ((self cando-structure))
  "mol2")

(defmethod get-structure-string ((self cando-structure))
  (check-type self cando-structure)
  (check-type (matter self) chem:aggregate)
  (progn
    (cljw:widget-log "Generating mol2 as string~%")
    (chem:aggregate-as-mol2-string (matter self)))
  #++(progn
       (cljw:widget-log "Saving structure to /tmp/structure.mol2~%")
  
       (cando:save-mol2 (matter self) "/tmp/structure.mol2" :use-sybyl-types t)
       (with-open-file (stream "/tmp/structure.mol2" :direction :input)
	 (let* ((entire-file (make-string (+ (file-length stream) 2)
					  :initial-element #\newline)))
	   (read-sequence entire-file stream)
	   (close stream)
	   entire-file))))



(defclass TextStructure (Structure)
  ((text :initarg :text :accessor text :initform nil)
   (ext :initarg :ext :accessor ext :initform "pdb")
   (path :accessor path :initform "")
   (params :initarg params :accessor params :type list :initform ())))
  
(defmethod get-structure-string ((self TextStructure))
  (text self))

(defclass RdkitStructure (Structure)
  ((rdkit-mol :initarg :rdkit-mol :accessor rdkit-mol :initform nil)
   (ext :initarg :ext :accessor ext
	:initform "pdb")
   (path :accessor path :initform "")
   (params :accessor params :type list :initform ())))

(defmethod get-structure-string ((self RdkitStructure))
  (error "adaptor::get-structure-string Implement me!!!!"))
#|
 from rdkit import Chem
        fh = StringIO(Chem.MolToPDBBlock(self.-rdkit_mol))
        return fh.read()
|#

(defclass PdbIdStructure (Structure)
  ((pdbid :initarg :pdbid :accessor pdbid :initform nil)
   (ext :accessor ext :initform "cif")
   (params :accessor params :type list :initform ())))

(defmethod get-structure-string ((self PdbIdStructure))
  (let ((url (concatenate 'string "http://files.rcsb.org/view/" (pdbid self) ".cif")))
    (cljw:widget-log "About to get-structure-string from ~s~%" url)
    (destructuring-bind (response header stream)
	(trivial-http:http-get url)
      (let ((contents (with-output-to-string (sout)
			(trivial-http::copy-stream stream sout))))
	(cljw:widget-log "Read url: ~s~%" url)
	(cljw:widget-log "     response: ~a~%" response)
	(cljw:widget-log "       header: ~s~%" header)
	(cljw:widget-log "      contents are not show - may be too long~%")
	(close stream)
	contents))))

#|
    def get-structure-string(self):
        url = "http://www.rcsb.org/pdb/files/" + self.pdbid + ".cif"
        return urlopen(url).read()
|#

(defclass ASEStructure (Structure)
  ((ase-atoms :initarg :ase-atoms :accessor ase-atoms
	      :initform nil)
   (path :accessor path
	 :initform "")
   (params :initarg params :accessor params :type list :initform ())
   (ext :initarg :ext :accessor ext :initform "pdb")))

(defmethod get-structure-string ((self ASEStructure))
  (error "ASEStructure::get-structure-string help!!"))
#|
  def get-structure-string(self):
        with tempfolder():
            self.-ase_atoms.write('tmp.pdb')
            return open('tmp.pdb').read()
|#

(defclass SimpletrajTrajectory (Trajectory Structure)
  ((path :initarg :path :accessor path :initform nil)
   (structure-path :initarg :structure-path :accessor structure-path :initform path)
   (traj-cache :accessor traj-cache :initform nil);HELP!!! Please help
   (ext :accessor ext :initform nil) ;HELP!!! Please help me
   (params :accessor params :type list :initform nil)
   (trajectory :accessor trajectory :initform nil)
   (id :accessor id :initform (format nil "~W" (uuid:make-v4-uuid)))))

(defmethod get-coordinates ((self SimpletrajTrajectory) index)
  (error "Implement me!!! get-coordinates SimpletrajTrajectory!"))
#|
    def get-coordinates(self, index):
        traj = self.traj_cache.get(os.path.abspath(self.path))
        frame = traj.get_frame(index)
        return frame["coords"]
|#

(defmethod get-structure-string ((self SimpletrajTrajectory))
  (error "help get-structure-string of simpletrajtrajectory"))
  ;;;return open(self._structure_path).read()

(defmethod n-frames ((self SimpletrajTrajectory))
  (error "n-frames simpletrajtrajectory needs some help"))
;;; traj = self.traj_cache.get(os.path.abspath(self.path))
;;; return traj.numframes

(defclass MDTrajTrajectory (Trajectory Structure)
  ((trajectory :initarg :trajectory :accessor trajectory
	       :initform nil)
   (ext :accessor ext :initform "pdb")
   (params :accessor params :type list :initform ())
   (id :accessor id :initform (format nil "~W" (uuid:make-v4-uuid)))))

(defmethod get-coordinates ((self MDTrajTrajectory) index)
  (* 10 (aref (xyz (trajectory self)) index)))

(defmethod n-frames ((self MDTrajTrajectory))
  (n-frames (trajectory self)))

(defmethod get-structure-string ((self MDTrajTrajectory))
  (error "Help the get-structure-String MDTrajTrajectory"))
#|
 def get-structure-string(self):
        fd, fname = tempfile.mkstemp()
        self.trajectory[0].save_pdb(fname)
        pdb_string = os.fdopen(fd).read()
        # os.close( fd )
        return pdb_string
|#

(defclass PyTrajTrajectory (Trajectory Structure)
  ((trajectory :initarg :trajectory :accessor trajectory
	       :initform nil)
   (ext :accessor ext :initform "pdb")
   (params :accessor params :type list :initform ())
   (id :accessor id :initform (format nil "~W" (uuid:make-v4-uuid)))))

(defmethod get-coordinates ((self PyTrajTrajectory) index)
  (xyz (aref (trajectory self) index)))

(defmethod n-frames ((self PyTrajTrajectory))
  (n-frames (trajectory self)))

(defmethod get-structure-string ((self PyTrajTrajectory))
  (error "PyTrajTrajectory get-structure-string error"))
#|
    def get-structure-string(self):
        fd, fname = tempfile.mkstemp(suffix=".pdb")
        self.trajectory[:1].save(fname, format="pdb", overwrite=True)
        pdb_string = os.fdopen(fd).read()
        # os.close( fd )
        return pdb_string

|#
;;;There's something fishy goin on here. Check python code listed below.
(defclass ParmEdTrajectory (Trajectory Structure)
  ((trajectory :initarg :trajectory :initform nil :accessor trajectory)
   (ext :accessor ext :initform "pdb")
   (params :accessor params :type list :initform ())
   (xyz :accessor xyz :initform nil)
   (id :accessor id :initform (format nil "~W" (uuid:make-v4-uuid)))
   (only-save-1st-model :accessor only-save-1st-model :type bool :initform :true)))

#|

@register_backend('parmed')
class ParmEdTrajectory(Trajectory, Structure):
    '''ParmEd adaptor.
    '''

    def __init__(self, trajectory):
        self.trajectory = trajectory
        self.ext = "pdb"
        self.params = {}
        # only call get_coordinates once
        self._xyz = trajectory.get_coordinates()
        self.id = str(uuid.uuid4())
        self.only_save_1st_model = True

    def get_coordinates(self, index):
        return self._xyz[index]

    @property
    def n_frames(self):
        return len(self._xyz)

    def get-structure-string(self):
        fd, fname = tempfile.mkstemp(suffix=".pdb")
        # only write 1st model
        if self.only_save_1st_model:
            self.trajectory.save(
                fname, overwrite=True,
                coordinates=self.trajectory.coordinates)
        else:
            self.trajectory.save(fname, overwrite=True)
        pdb_string = os.fdopen(fd).read()
        # os.close( fd )
        return pdb_string
|#

;(defmethod initialize-instance :after ((self ParmEdTrajectory) &key)
;;  (setf (xyz self) ((get_coordinates

(defclass MDAnalysisTrajectory (Trajectory Structure)
  ((atomgroup :initarg :atomgroup :accessor atomgroup)
   (ext :accessor ext :initform "pdb")
   (params :accessor params :type list :initform ())
   (id :accessor id :initform (format nil "~W" (uuid:make-v4-uuid)))))

(defmethod get-coordinates ((self MDAnalysisTrajectory) index)
  (aref (trajectory (universe (atomgroup self))) index)
  (positions (atoms (atomgroup self))))

(defmethod n-frames ((self MDAnalysisTrajectory))
  (n-frames (trajectory (universe (atomgroup self)))))

(defmethod get-structure-string ((self MDAnalysisTrajectory))
  (error "help MDAnalysisTrajectory get-structure-string"))

#|
  def get-structure-string(self):
        try:
            import MDAnalysis as mda
        except ImportError:
            raise ImportError(
                "'MDAnalysisTrajectory' requires the 'MDAnalysis' package"
            )
        u = self.atomgroup.universe
        u.trajectory[0]
        f = mda.lib.util.NamedStream(StringIO(), 'tmp.pdb')
        atoms = self.atomgroup.atoms
        # add PDB output to the named stream
        with mda.Writer(f, atoms.n_atoms, multiframe=False) as W:
            W.write(atoms)
        # extract from the stream
        pdb_string = f.read()
        return pdb_string

|#

(defclass HTMDTrajectory (Trajectory)
  ((mol :initarg :mol :accessor mol)
   (ext :accessor ext :initform "pdb")
   (params :accessor params :type list :initform ())
   (id :accessor id :initform (format nil "~W" (uuid:make-v4-uuid)))))

(defmethod get-coordinates ((self HTMDTrajectory) index)
  (error "help get-coordinates HTMDTrajectory"))
#|
    def get_coordinates(self, index):
        return np.squeeze(self.mol.coords[:, :, index])
|#

(defmethod n-frames ((self HTMDTrajectory))
  (numFrames (mol self)))

(defmethod get-structure ((self HTMDTrajectory))
  (error "help get-structure of HTMDTrajectory"))
#|
    def get-structure-string(self):
        import tempfile
        fd, fname = tempfile.mkstemp(suffix='.pdb')
        self.mol.write(fname)
        pdb_string = os.fdopen(fd).read()
        # os.close( fd )
        return pdb_string
|#
