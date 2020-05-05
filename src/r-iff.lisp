(in-package :cl-user)

(defpackage :r-iff
  (:use :cl)
  (:export ;;;; MAIN API
           ;;; IFF
           #:iff ; import.
           #:write-iff ; export.
           ;;; RIFF
           #:riff ; import.
           #:write-riff ; export.
           ;;;; For heavy user.
           ;;; CHUNK class
           #:chunk
           #:leaf
           #:node
           #:group
           ;; Slots
           #:id
           #:data
           ;; Slot accessor.
           #:id<-chunk ; reader
           #:data<-chunk ; accessor
           ;;; as DSL.
           #:defparser
           ;; Parsers.
           ;; #:leaf
           ;; #:node
           ;; #:group
           ;;;; Special variable to control behavior.
           #:*read-data-element-type*
           #:*default-class*
           ;; Magic numbers
           #:+size-of-id+
           #:+size-of-header+
           ;; Helper generic functions.
           #:write-chunk
           #:compute-length
           #:retrieve
           ;; Utilities
           #:read-string
           #:read-data
           #:ensure-even))

(in-package :r-iff)

;;;; Example Diagrams from
#| http://www.martinreddy.net/gfx/2d/IFF.txt

 +-----------------------------------+
 |'FORM'                 24070       | FORM 24070 IBLM ; header 8 byte.
 +-----------------------------------+                          +
 |'ILBM'                             |                 ; header 4 byte.
 +-----------------------------------+                          +
 | +-------------------------------+ |
 | | 'BMHD'20                      | | .BMHD 20        ; header 8 byte.
 | | 320, 200, 0, 0, 3, 0, 0, ...  | |                 ; data   20 byte.
 | + ------------------------------+ |                          +
 | | 'CMAP' 21                     | | .CMAP 21        ; header 8 byte.
 | | 0, 0, 0 32, 0, 0 64,0,0 ..    | |                 ; data   21 byte.
 | +-------------------------------+ |                          +
 | 0                                 |                 ; padd   1 byte.
 +-----------------------------------+                          +
 |'BODY'  24000                      | .BODY 24000     ; header 8 byte.
 |0, 0, 0, ...                       |                 ; data   24000 byte.
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

(defvar *length-reader* #'nibbles:read-ub32/be)

(defvar *length-writer* #'nibbles:write-ub32/be)

(defvar *default-parser* 'leaf)

(defparameter *data-size-limit* array-total-size-limit)

(defparameter *read-data-element-type* '(unsigned-byte 8))

(defparameter *default-class* nil)

;;;; HELPERS

(defun read-string (stream size)
  (let ((it (nibbles:make-octet-vector size)))
    (assert (= (read-sequence it stream) size) () 'end-of-file :stream stream)
    (babel:octets-to-string it)))

(defun read-id (stream) (read-string stream +size-of-id+))

(defun parser<-id (thing) (gethash thing *iff-parsers* *default-parser*))

(defun rewind (stream num)
  (file-position stream (- (file-position stream) num)))

(defun read-length (stream) (funcall *length-reader* stream))

(defun ensure-even (integer)
  (if (oddp integer)
      (1+ integer)
      integer))

;;; CLHS say array-total-size-limit is "implementation-dependent, but which is not less than 1024."

(defun read-data (stream size)
  (check-type stream stream) ; <--- CCL needs.
  (loop :for length := (min *data-size-limit* size)
        :for chunk
             := (make-array (list length)
                            :element-type *read-data-element-type*)
        :do (assert (= length (read-sequence chunk stream)) ()
              'end-of-file :stream stream)
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
       :reader id<-chunk)
   (data :initarg :data :accessor data<-chunk))
  (:documentation "Abstract superclass of group, node, and leaf."))

(defclass leaf (chunk) ((data :type list)))

(defmethod initialize-instance ((o leaf) &key id stream size)
  (with-slots ((chunk-id id) (chunk-data data))
      o
    (setf chunk-id id
          chunk-data (read-data stream size)))
  (when (oddp size)
    (read-byte stream)) ; Consume padd.
  o)

(defclass node (chunk) ((data :type list)))

(defclass group (chunk) ((data :type (or null node))))

;;; print object

(defmethod print-object ((chunk chunk) stream)
  (print-unreadable-object (chunk stream)
    (format stream "~A ~S" (type-of chunk) (id<-chunk chunk))))

;;; iff parsers

(declaim
 (ftype (function (stream &optional (or null (integer 4 #xFFFFFFFF)))
         (values chunk (integer 4 #xFFFFFFFF) &optional))
        leaf
        node
        group))

(defun leaf (stream &optional end)
  "Parser for leaf chunk."
  (declare (ignore end))
  (let* ((id (read-id stream))
         (length (read-length stream)) ; leaf must have length.
         (padded-size (ensure-even length)))
    (values
      (make-instance (or *default-class* 'leaf)
                     :id id
                     :stream stream
                     :size length)
      (+ +size-of-header+ padded-size))))

(defun node (stream &optional end)
  "Parser for node chunk."
  (let ((id (read-id stream))) ; node does not have length.
    (values
      (make-instance (or *default-class* 'node)
                     :id id
                     :data (make-chunks stream (- end +size-of-id+)))
      end)))

(defun group (stream &optional end)
  "Parser for group chunk"
  (declare (ignore end))
  (let ((id (read-id stream))
        (length (read-length stream)) ; group must have length.
        )
    (values
      (make-instance (or *default-class* 'group)
                     :id id
                     :data (unless (zerop length)
                             (node stream length)))
      (+ +size-of-header+ length))))

(defun make-chunks (stream end &optional acc)
  "make-chunk recursively."
  (if (zerop end)
      (nreverse acc)
      (multiple-value-bind (chunk count)
          (make-chunk stream end)
        (make-chunks stream (- end count) (cons chunk acc)))))

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

(defgeneric compute-length
    (chunk)
  (:method ((chunk leaf))
   (+ +size-of-header+ (reduce #'+ (data<-chunk chunk) :key #'length)))
  (:method ((chunk node))
   (+ +size-of-header+
      (reduce #'+ (data<-chunk chunk)
              :key (lambda (elt) (ensure-even (data<-chunk elt))))))
  (:method ((chunk group))
   (let ((data (data<-chunk chunk)))
     (if data
         (+ +size-of-header+ (compute-length data))
         +size-of-header+))))

(defgeneric write-chunk
    (chunk stream)
  (:method :around ((chunk chunk) stream)
   (write-sequence (babel:string-to-octets (id<-chunk chunk)) stream)
   (call-next-method))
  (:method :before ((chunk leaf) stream)
   (funcall *length-writer* (- (compute-length chunk) +size-of-header+) stream))
  (:method :before ((chunk group) stream)
   (funcall *length-writer* (- (compute-length chunk) +size-of-header+) stream))
  (:method ((chunk leaf) stream)
   (let ((length 0))
     (dolist (vector (data<-chunk chunk))
       (incf length (length vector))
       (write-sequence vector stream))
     (when (oddp length)
       (write-byte 0 stream)))
   chunk)
  (:method ((chunk node) stream)
   (dolist (chunk (data<-chunk chunk)) (write-chunk chunk stream)) chunk)
  (:method ((chunk group) stream)
   (let ((data (data<-chunk chunk)))
     (when data
       (write-chunk data stream)))
   chunk))

;;;; DEFPARSER
#| SYNTAX
 | (defparser name parser &key default-class)
 |
 | name := (vector character 4)
 |
 | parser := (function (stream &optional end) (values chunk end))
 | end := (integer 0 *)
 |
 | default-class := symbol which names subclass of chunk.
 |
 |#

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro defparser (name parser &key default-class)
    ;; Trivial syntax check.
    (check-type name id)
    `(progn
      (setf (gethash ,name *iff-parsers*)
              ,(if default-class
                   `(lambda (&rest #0=#:args)
                      (let ((*default-class* ',default-class))
                        (apply ,parser #0#)))
                   parser))
      ,name)))

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
    (let ((id (read-id stream)) (*length-reader* 'nibbles:read-ub32/be))
      (assert (find id '("FORM" "LIST" "CAT ") :test #'equal))
      (rewind stream +size-of-id+)
      (make-chunk stream))))

;;; EXPORT

(defun write-iff (iff stream)
  (let ((*length-writer* 'nibbles:write-ub32/be))
    (write-chunk iff stream)))

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

;;;; HELPER

(defgeneric retrieve
    (id chunk)
  (:method ((id string) (chunk leaf))
   (when (equal id (id<-chunk chunk))
     (list chunk)))
  (:method ((id string) (chunk node))
   (let ((data
          (mapcan (lambda (chunk) (retrieve id chunk)) (data<-chunk chunk))))
     (if (equal id (id<-chunk chunk))
         (cons chunk data)
         data)))
  (:method ((id string) (chunk group))
   (if (equal id (id<-chunk chunk))
       (cons chunk (retrieve id (data<-chunk chunk)))
       (retrieve id (data<-chunk chunk)))))
