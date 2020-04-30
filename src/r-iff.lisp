(in-package :cl-user)

(defpackage :r-iff
  (:use :cl)
  (:export ;; chunk class
           #:chunk
           ;; chunk predicates
           #:chunkp
           #:groupp
           #:nodep
           #:leafp
           ;; chunk slots
           #:id
           #:data
           #:length
           #:src-path
           ;; chunk accessors
           #:id<-chunk
           #:length<-chunk
           #:src-path<-chunk
           #:data<-chunk
           ;; iff class and its accessors.
           #:iff
           #:id<-iff
           #:length<-iff
           #:data<-iff
           ;; riff class and its accessors.
           #:riff
           #:id<-riff
           #:length<-riff
           #:data<-riff
           ;; as DSL.
           #:defparser
           #:leaf
           #:node
           #:group
           #:*length-reader*
           #:*length-writer*
           ;; magic numbers
           #:+size-of-id+
           #:+size-of-header+
           ;; useful API
           #:make-chunk
           #:make-chunks
           #:write-iff
           #:write-chunk
           #:write-chunks
           #:compute-chunk-length
           #:retrieve
           ;; utilities
           #:ensure-even))

(in-package :r-iff)

;;;; Example Diagrams from
#| http://www.martinreddy.net/gfx/2d/IFF.txt

 +-----------------------------------+
 |'FORM'                 24070       | FORM 24070 IBLM
 +-----------------------------------+
 |'ILBM'                             |
 +-----------------------------------+
 | +-------------------------------+ |
 | | 'BMHD'20                      | | .BMHD 20
 | | 320, 200, 0, 0, 3, 0, 0, ...  | |
 | + ------------------------------+ |
 | | 'CMAP' 21                     | | .CMAP 21
 | | 0, 0, 0 32, 0, 0 64,0,0 ..    | |
 | +-------------------------------+ |
 | 0                                 |
 +-----------------------------------+
 |'BODY'  24000                      | .BODY 24000
 |0, 0, 0, ...                       |
 +-----------------------------------+

 +---------------------------------------+
 |'LIST'  48114                          | LIST 48114 AAAA
 +---------------------------------------+
 |'AAAA'                                 | .PROP 62 ILBM
 |+-----------------------------------+  |
 ||'PROP'  62                         |  |
 |+-----------------------------------+  |
 ||'ILBM'                             |  |
 |+-----------------------------------+  |
 || +-------------------------------+ |  |
 || | 'BMHD' 20                     | |  | ..BMHD 20
 || | 320, 200, 0, 0, 3, 0, 0, ...  | |  |
 || | ------------------------------+ |  |
 || | 'CMAP' 21                     | |  | ..CMAP 21
 || | 0, 0, 0 32, 0, 0 64,0,0 ..    | |  |
 || +-------------------------------+ |  |
 || 0                               | |  |
 |+-----------------------------------+  |
 |+-----------------------------------+  |
 ||'FORM'  24012                      |  | .FORM 24012 ILBM
 |+-----------------------------------+  |
 ||'ILBM'                             |  |
 |+-----------------------------------+  |
 || +-----------------------------+   |  |
 || |'BODY'  24000                |   |  | ..BODY 24000
 || |0, 0, 0, ...                 |   |  |
 || +-----------------------------+   |  |
 |+-----------------------------------+  |
 |+-----------------------------------+  |
 ||'FORM'  24012                      |  | .FORM 24012 ILBM
 |+-----------------------------------+  |
 ||'ILBM'                             |  |
 |+-----------------------------------+  |
 || +-----------------------------+   |  |
 || |'BODY'  24000                |   |  | ..BODY 24000
 || |0, 0, 0, ...                 |   |  |
 || +-----------------------------+   |  |
 |+-----------------------------------+  |
 +---------------------------------------+
|#

;;;; too complex! but we have S-Expression!

#+example
(form size (ilbm (bmhd size data) (cmap size data pad) (body size data)))

#+example
(list size
      (aaaa (prop size (ilbm (mbhd size data) (cmap size data pad)))
       (form size (ilbm (body size data))) (form size (ilbm (body size data)))))

;;;; Pretty simple! Adding one more abstruction!

#+example
(group size (node (leaf size data) (leaf size data pad) (leaf size data)))

#+example
(group size
       (node (group size (node (leaf size data) (leaf size data pad)))
             (group size (node (leaf size data)))
             (group size (node (leaf size data)))))

#| Conclusion
 |
 | * Every parenthesis represents instance of CHUNK.
 | * LEAF has size, respond to set PAD or not, and has data.
 | * NODE never have size, respond to how handle its leaves.
 | * GROUP has size, and has only one NODE.
 |
 |#

;;;; implementation
;;;; SPECIALS
;;; types

(deftype id () '(vector standard-char 4))

(defclass chunk ()
  ((id :initarg :id
       :initform (error "ID is required")
       :type id
       :reader id<-chunk
       :documentation "Required")
   (length :initarg :length
           :type (signed-byte 32)
           :reader length<-chunk
           :documentation "Only leaf or group has.")
   (src-path :initarg :src-path
             :type (or string pathname)
             :reader src-path<-chunk
             :documentation "Only leaf has.")
   (data :initarg :data
         :reader data<-chunk
         :documentation "Gourp contains node. Node contains leaves. Leaf never contains chunk."))
  (:documentation "Basic type of group, node, and leaf."))

;;; print object

(defmethod print-object ((chunk chunk) stream)
  (print-unreadable-object (chunk stream)
    (let ((type (chunk-type chunk)))
      (format stream "~A ~A~@[ : ~A~]" type (id<-chunk chunk)
              (when (eq :leaf type)
                (data<-chunk chunk))))))

;;; predicates

(defun chunkp (arg) "Evaluated to T when ARG is chunk." (typep arg 'chunk))

(defun nodep (arg)
  "Evaluated to T when ARG is node."
  (and (chunkp arg) (not (slot-boundp arg 'length))))

(defun leafp (arg)
  "Evaluated to T when ARG is leaf."
  (and (chunkp arg) (slot-boundp arg 'src-path)))

(defun groupp (arg)
  "Evaluated to T when ARG is group."
  (and (chunkp arg) (slot-boundp arg 'data) (chunkp (data<-chunk arg))))

;;; iff parsers

(defvar *iff-parsers* (make-hash-table :test #'equal))

(defvar *length-reader*
  #'nibbles:read-sb32/be
  "Default value for being used by import-file")

(defvar *length-writer*
  #'nibbles:write-sb32/be
  "Default value for being used by export-file")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro defparser (name parser)
    "[syntax]defparser name parser
    NAME is r/iff's tag, must be string.
    PARSER is function. Its lambda-list must be (stream &optional end)."
    (check-type name string)
    `(progn (setf (gethash ,name *iff-parsers*) ,parser) ,name)))

(defconstant +size-of-id+ 4)

(defconstant +size-of-header+ 8) ; id + length

(defun leaf (stream &optional end)
  "Parser for leaf chunk."
  (declare (ignore end))
  (let* ((id (read-id stream))
         (length (read-length stream)) ; leaf must have length.
         (current-position (file-position stream))
         (padded-size (ensure-even length)))
    (skip stream padded-size)
    (values
      (make-instance 'chunk
                     :id (string<-id id)
                     :length length
                     :src-path (truename (pathname stream))
                     :data current-position)
      (+ +size-of-header+ padded-size))))

(defun node (stream end)
  "Parser for node chunk."
  (let ((id (read-id stream))) ; node does not have length.
    (values
      (make-instance 'chunk
                     :id (string<-id id)
                     :data (make-chunks stream (- end +size-of-id+)))
      end)))

(defun group (stream &optional end)
  "Parser for group chunk"
  (declare (ignore end))
  (let ((id (read-id stream)) (length (read-length stream))) ; group must have
                                                             ; length.
    (values
      (make-instance 'chunk
                     :id (string<-id id)
                     :length length
                     :data (make-chunk stream length))
      (+ +size-of-header+ length))))

(defun make-chunks (stream end &optional acc)
  "make-chunk recursively."
  (multiple-value-bind (chunk count)
      (make-chunk stream end)
    (let ((counter (- end count)))
      (if (zerop counter)
          (values (nreverse (push chunk acc)) end)
          (make-chunks stream counter (push chunk acc))))))

(defvar *default-parser* #'leaf)

(defun make-chunk (stream &optional end)
  "Top level."
  (let* ((id (read-id stream)))
    (if (eql end 4) ; as id-only-p
        (values (make-instance 'chunk :id (string<-id id)) 4)
        (progn
         (rewind stream +size-of-id+)
         (let ((parser (parser<-id id)))
           (if parser
               (funcall parser stream end)
               (progn
                (warn "~S is unknown ID" (string<-id id))
                (funcall *default-parser* stream end))))))))

;; helpers

(defun read-length (stream) (funcall *length-reader* stream))

(defun ensure-even (integer)
  (if (oddp integer)
      (1+ integer)
      integer))

(defun read-id (stream)
  (let ((it (nibbles:make-octet-vector +size-of-id+)))
    (read-sequence it stream)
    it))

(defun string<-id (id-vector) (map 'string #'code-char id-vector))

(defun parser<-id (id-vector) (gethash (string<-id id-vector) *iff-parsers*))

(defun skip (stream num) (file-position stream (+ (file-position stream) num)))

(defun rewind (stream num)
  (file-position stream (- (file-position stream) num)))

(defparser "FORM" #'group)

(defparser "LIST" #'group)

(defparser "PROP" #'group)

(defparser "CAT " #'group)

(defparser "RIFF" #'group)

(defparser "JJJJ" #'node)

(defun chunk-type (chunk)
  (cond ((groupp chunk) :group)
        ((nodep chunk) :node)
        ((leafp chunk) :leaf)
        (t (error "Unknown type ~S" chunk))))

#|
;;; iff file types
(defclass iff(file)
  ((data :initarg :data :type chunk
	 :accessor data<-iff :accessor Data<-file)))
(defclass riff(iff)
  ((data :initarg :data :type chunk
	 :accessor data<-riff :accessor Data<-file
	 :accessor data<-iff)))

(define-file-type"iff":iff)
(define-file-type"riff" :riff)

;;; import
(defmethod %Import-file((type(eql :iff))path)
  "Interface for usability."
  (%Import-file(make-instance 'iff)path)) ; as diverging.

(defmethod %Import-file((type(eql :riff))path)
  "Interface for usability."
  (let((*length-reader* #'Read-ub32/le)) ; as prepare.
    (%Import-file (make-instance 'riff) path))) ; as diverging.

(defmethod %Import-file((file iff)path)
  "Actual implementation.
  FILE may iff or riff."
  (with-open-file(s path :element-type 'Octet)
    (let((id(read-id s)))
      (if(toplevel-id-p file(string<-id id))
	(progn (rewind s +size-of-id+)
	       (setf(Data<-file file)(make-chunk s))
	       file)
	(error "Invalid ~A file : ~S"
	       (type-of file)
	       (string<-id id))))))

(defun toplevel-id-p(type arg)
  "Evaluated to T when arg is toplevel id."
  (and(typep arg 'id)
    (typecase type
      (riff
       (find arg #(riff list) :test #'string=))
      (iff
       (find arg #(form list |CAT |) :test #'string=)))))

;;; export
(defmethod Export-file((iff iff)path)
  (with-open-file(dest path :direction :output 
		      :if-exists :supersede
		      :if-does-not-exist :create 
		      :element-type 'octet)
    (write-iff iff dest)))

(defmethod Export-file((riff riff)path)
  (declare(ignore path))
  (let((*length-writer* #'Write-ub32/le))
    (call-next-method)))

(defun write-iff(iff dest)
  "write r/iff data to DEST stream."
  (write-chunk(data<-iff iff)dest))

(defun write-chunk(chunk dest)
  (with-slots(id length data src-path)chunk
    (write-sequence(String-to-octets id)dest)
    (ecase(chunk-type chunk)
      (:leaf (funcall *length-writer* length dest)
	     (with-open-file(src src-path :element-type 'Octet)
	       (file-position src data)
	       (loop repeat length
		     do (write-byte(read-byte src)dest)))
	     (when(oddp length) ; pad-needed-p
	       (write-byte 0 dest)))
      (:node (write-chunks data dest))
      (:group (funcall *length-writer* length dest)
	      (if(= 4 length)
		(write-sequence(String-to-octets(id<-chunk data))dest)
		(write-chunk data dest))))))

(defun write-chunks(chunks dest)
  (dolist(chunk chunks)
    (write-chunk chunk dest)))

(defun retrieve(target iff)
  "Retrieve TARGET id chunk from IFF."
  (labels((REC(chunk)
	    (with-slots(id data)chunk
	      (if(string= id target)
		(return-from retrieve chunk)
		(cond
		  ((leafp chunk)nil)
		  ((nodep chunk)(and(slot-boundp chunk 'data)
				  (map nil #'REC data)))
		  (t(REC data)))))))
    (REC(data<-iff iff))))

(defun compute-chunk-length(chunk)
  "Evaluate total chunk size, not only length."
  (if(not(nodep chunk))
    (+ +size-of-header+(ensure-even(length<-chunk chunk)))
    (+ +size-of-id+(reduce #'+(and(slot-boundp chunk 'data)
				(data<-chunk chunk))
			   :key #'compute-chunk-length))))

(defmethod Compute-file-size((file iff))
  (compute-chunk-length(Data<-file file)))

(defmethod Merge-files((type(eql :iff))&rest files)
  (make-instance
    'iff
    :id "CAT "
    :length(+ +size-of-header+ (reduce #'+ files :key #'compute-file-size))
    :data(make-instance
	   'chunk
	   :id "JJJJ"
	   :data (mapcar #'data<-file files))))
|#

