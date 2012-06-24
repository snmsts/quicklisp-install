(in-package :ql-install)

#+quicklisp
(defvar *install-directory* 
  (merge-pathnames "quicklisp-install/" (first (last ql:*local-project-directories*))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun qimerge (name)
    (merge-pathnames name *install-directory*)))

(defvar *dist-path* (qimerge "dist/"))
(defvar *archive-path* (qimerge "archives/"))

(defun urlencode (url)
  (drakma::url-encode url :utf8))

(defun urldecode (url)
  (flexi-streams:octets-to-string
   (loop :for i :from 0 :below (length url)
      :for char := (aref url i)
      :collect 
      (cond ((or (char<= #\0 char #\9)
		 (char<= #\a char #\z)
		 (char<= #\A char #\Z)
		 (find char "$-_.!*'()," :test #'char=))
	     (char-code char))
	    ((char= char #\+)
	     #.(char-code #\Space))
	    ((char= char #\%)
	     (parse-integer  (subseq url (+ i 1) (+ (incf i 2) 1)) :radix 16))
	    (t (error "er"))))
   :external-format :utf8))

(defun download-url (url)
  (let* ((dir  *archive-path*)
	 (path (merge-pathnames (urlencode url) dir)))
    (ensure-directories-exist dir)
    (with-open-file (out path 
		      :element-type '(unsigned-byte 8)
		      :direction :output
		      :if-exists :supersede)
      (write-sequence (drakma:http-request url
		       :force-binary t
		       :preserve-uri t)
       out))
    path))

(defun list-installed ()
  (remove "output.tar" 
	  (mapcar (lambda (x) 
		    (urldecode (format nil "~A.~A"
				       (pathname-name x)
				       (pathname-type x))))
		  (directory (merge-pathnames  "*.*" *archive-path*)))
	  :test 'equal))

(defun uninstall (dir)
  #+quicklisp
  (ql-util:delete-file-if-exists (merge-pathnames dir *archive-path*))
  #+quicklisp
  (ql-impl-util:delete-directory-tree
   (merge-pathnames (format nil "~A/" dir) *dist-path*)))

(defun tgz (url &key uninstall)
  #+quicklisp
  (if uninstall
      (uninstall (urlencode url))
      (let* ((path (download-url url))
	     (output (merge-pathnames "output.tar"(make-pathname :defaults path :name nil :type nil)))
	     (extract (merge-pathnames (format nil "~A/" (urlencode url)) *dist-path*)))
	(ql-gunzipper:gunzip path output)
	(ensure-directories-exist extract)
	(ql-minitar:unpack-tarball output :directory extract)
	(ql:register-local-projects)))
  #-quicklisp
  (error "quicklisp not installed"))

(defun github (user name &key uninstall)
  "inspired from http://cadr.g.hatena.ne.jp/g000001/20100628/1277731696"
  (tgz (format nil
	       "http://github.com/~A/~A/tarball/master"
	       user name) :uninstall uninstall))

#|
(ql-install:tgz "http://sourceforge.jp/frs/redir.php?m=jaist&f=%2Figo%2F55030%2Fcl-igo-0.3.1.tar.gz")
(ql-install:tgz "https://github.com/downloads/sile/charseq/charseq-0.1.8.tar.gz")
(ql:quickload :igo)
|#
