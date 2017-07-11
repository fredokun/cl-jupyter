(in-package :nglv)

(defun install-nglview-js-widgets (&rest kwargs &key (user t) (symlink nil) (overwrite t) (debug nil) &allow-other-keys)
  (let ((nvlivew-js-dirs (apply #'install-nbextension-python nbextensions "nglview" :user user :symlink symlink :overwrite overwrite kwargs)));Not totally sure about that... I'm using nbextensions as the object like hopefully they'll have a self component when we write it? ah. 
    (if debug
	(progn
	(print nglivew-js-dirs)
	(error "help finish my implementation! install-nglview-js-widgets")))))
       	;;;print([glob(join(my_dir, '*')) for my_dir in nglivew_js_dirs])

(defun enable-nglview-js-widgets (&optional (user t))
  (enable-nbextenstion-python nbextensions "nglview" :user user))

#||
(if (string= --name-- "--main--")
    (let ((parser (ArgumentParser argparse :description "nglview-js-widgets")))
      (add_argument parser "-u" "--user" :help "Install as current user instead of system-wide"
		    :action "store_true")
      (add_argument parser "-s" "--symlink" :help "Symlink instead of copying files"
		    :action "store_true")
      (add_argument parser "-f" "--force" :help "Overwrite any previously-installed files for this extenstion" :action "store_true")
      (add_argument parser "-d" "--debug" :help "print nglview-js-widgets" :action "store_true")
      (let args (parse_args parser)
	   (install_nglview_js_widgets :user (user args) :symlink (symlink args) :overwrite (force args) :debug (debug args))
	   (enable_nglview_js_widgets))))
    
  ||#
    
