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

(defconstant +size-of-id+ 4)

(defconstant +size-of-header+ 8) ; id + length

(defvar *iff-parsers* (make-hash-table :test #'equal))

(defvar *length-reader* #'nibbles:read-sb32/be)

(defvar *length-writer* #'nibbles:write-sb32/be)

(defvar *default-parser* 'leaf)

(defparameter *data-size-limit* array-total-size-limit)

;;;; HELPERS

(defun read-id (stream)
  (let ((it (nibbles:make-octet-vector +size-of-id+)))
    (read-sequence it stream)
    (map 'string #'code-char it)))

(defun parser<-id (thing) (gethash thing *iff-parsers* *default-parser*))

(defun skip (stream num) (file-position stream (+ (file-position stream) num)))

(defun rewind (stream num)
  (file-position stream (- (file-position stream) num)))

(defun read-length (stream) (funcall *length-reader* stream))

(defun ensure-even (integer)
  (if (oddp integer)
      (1+ integer)
      integer))

(defun read-data (stream size)
  (loop :for chunk
             := (make-array (list (min *data-size-limit* size))
                            :element-type '(signed-byte 8))
        :do (read-sequence chunk stream)
        :collect chunk
        :if (<= size *data-size-limit*)
          :do (loop-finish)
        :else
          :do (decf size *data-size-limit*)))

;;; types

(deftype id () '(vector character 4))

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

;;; print object

(defun chunk-type (chunk)
  (cond ((groupp chunk) :group)
        ((nodep chunk) :node)
        ((leafp chunk) :leaf)
        (t (error "Unknown type ~S" chunk))))

(defmethod print-object ((chunk chunk) stream)
  (print-unreadable-object (chunk stream)
    (let ((type (chunk-type chunk)))
      (format stream "~A ~A~@[ : ~A~]" type (id<-chunk chunk)
              (when (eq :leaf type)
                (data<-chunk chunk))))))

;;; iff parsers

(defun leaf (stream &optional end)
  "Parser for leaf chunk."
  (declare (ignore end))
  (let* ((id (read-id stream))
         (length (read-length stream)) ; leaf must have length.
         (padded-size (ensure-even length)))
    (skip stream padded-size)
    (values
      (make-instance 'chunk
                     :id id
                     :length length
                     :src-path (truename (pathname stream))
                     :data (read-data stream length))
      (+ +size-of-header+ padded-size))))

(defun node (stream end)
  "Parser for node chunk."
  (let ((id (read-id stream))) ; node does not have length.
    (values
      (make-instance 'chunk
                     :id id
                     :data (make-chunks stream (- end +size-of-id+)))
      end)))

(defun group (stream &optional end)
  "Parser for group chunk"
  (declare (ignore end))
  (let ((id (read-id stream)) (length (read-length stream))) ; group must have
                                                             ; length.
    (values
      (make-instance 'chunk
                     :id id
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

(defun make-chunk (stream &optional end)
  "Top level."
  (let* ((id (read-id stream)))
    (if (eql end 4) ; as id-only-p
        (values (make-instance 'chunk :id id) 4)
        (progn
         (rewind stream +size-of-id+)
         (multiple-value-bind (parser found?)
             (parser<-id id)
           (unless found?
             (warn "~S is unknown ID" id))
           (funcall parser stream end))))))

(defun write-chunk (chunk stream)
  ;; ID
  (write-sequence (babel:string-to-octets (id<-chunk chunk)) stream)
  ;; LENGTH
  (when (slot-boundp chunk 'length)
    (funcall *length-writer* (length<-chunk chunk) stream))
  ;; CONTENT
  (ecase (chunk-type chunk)
    (:leaf
     (dolist (vector (data<-chunk chunk)) (write-sequence vector stream))
     (when (oddp (length<-chunk chunk)) ; pad-needed-p
       (write-byte 0 stream)))
    (:node (dolist (chunk (data<-chunk chunk)) (write-chunk chunk stream)))
    (:group
     (if (= 4 (length<-chunk chunk))
         (write-sequence
           (babel:string-to-octets (id<-chunk (data<-chunk chunk))) stream)
         (write-chunk (data<-chunk chunk) stream))))
  ;; RETURN-VALUE
  chunk)

;;;; DEFPARSER
#| SYNTAX
 | (defparser name parser)
 |
 | name := (vector character 4)
 |
 | parser := (function (stream &optional end) (values chunk end))
 | end := (integer 0 *)
 |#

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro defparser (name parser)
    ;; Trivial syntax check.
    (check-type name id)
    `(progn (setf (gethash ,name *iff-parsers*) ,parser) ,name)))

(defparser "FORM" #'group)

(defparser "LIST" #'group)

(defparser "PROP" #'group)

(defparser "CAT " #'group)

(defparser "RIFF" #'group)

(defparser "JJJJ" #'node)

;;;; IFF
;;; IMPORT

(defun iff (pathname)
  (with-open-file (stream pathname :element-type 'nibbles:octet)
    (let ((id (read-id stream)))
      (assert (find id '("FORM" "LIST" "CAT ") :test #'equal))
      (rewind stream +size-of-id+)
      (make-chunk stream))))

;;; EXPORT

(defun write-iff (iff stream)
  (write-chunk iff stream))

;;;; RIFF
;;; IMPORT

(defun riff (pathname)
  (with-open-file (stream pathname :element-type 'nibbles:octet)
    (let* ((*length-reader* #'nibbles:read-ub32/le) (id (read-id stream)))
      (assert (find id '("RIFF" "LIST") :test #'equal))
      (rewind stream +size-of-id+)
      (make-chunk stream))))

;;; EXPORT

(defun write-riff (riff stream)
  (let ((*length-writer* #'nibbles:write-ub32/le))
    (write-chunk riff stream)))

#|
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

