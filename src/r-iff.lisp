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
           #:src-path
           ;; Slot accessor.
           #:id<-chunk ; accessor
           #:src-path<-chunk ; reader
           #:data<-chunk ; accessor
           ;;; as DSL.
           #:defparser
           ;; Constructors.
           ;; #:leaf
           ;; #:node
           ;; #:group
           ;;;; Special variable to control behavior.
           #:*read-data-element-type*
           #:*leaf-class*
           #:*node-class*
           #:*group-class*
           ;; Magic numbers
           #:+size-of-id+
           #:+size-of-header+
           ;; Helpers
           #:make-chunk
           #:make-chunks
           #:write-chunk
           #:find-by-id
           ;; Utilities
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

(defparameter *leaf-class* 'leaf)

(defparameter *node-class* 'node)

(defparameter *group-class* 'group)

;;;; HELPERS

(defun read-id (stream)
  (let ((it (nibbles:make-octet-vector +size-of-id+)))
    (assert (= (read-sequence it stream) +size-of-id+) ()
      'end-of-file :stream stream)
    (map 'string #'code-char it)))

(defun parser<-id (thing) (gethash thing *iff-parsers* *default-parser*))

(defun rewind (stream num)
  (file-position stream (- (file-position stream) num)))

(defun read-length (stream) (funcall *length-reader* stream))

(defun ensure-even (integer)
  (if (oddp integer)
      (1+ integer)
      integer))

(defun read-data (stream size)
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
       :accessor id<-chunk)
   (src-path :initarg :src-path
             :type (or string pathname)
             :reader src-path<-chunk)
   (data :initarg :data :accessor data<-chunk))
  (:documentation "Abstract superclass of group, node, and leaf."))

(defclass leaf (chunk) ((data :type list)))

(defmethod initialize-instance :after
           ((o leaf) &key stream size &allow-other-keys)
  (setf (data<-chunk o) (read-data stream size)))

(defclass node (chunk) ((data :type list)))

(defclass group (chunk) ((data :type node)))

;;; print object

(defmethod print-object ((chunk chunk) stream)
  (print-unreadable-object (chunk stream)
    (format stream "~A ~S" (type-of chunk) (id<-chunk chunk))))

;;; iff parsers

(declaim
 (ftype (function (stream &optional (integer 0 *))
         (values chunk (integer 0 *) &optional))
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
      (make-instance *leaf-class*
                     :id id
                     :position (file-position stream)
                     :stream stream
                     :size length)
      (+ +size-of-header+ padded-size))))

(defun node (stream &optional end)
  "Parser for node chunk."
  (let ((id (read-id stream))) ; node does not have length.
    (values
      (make-instance *node-class*
                     :id id
                     :data (make-chunks stream (- end +size-of-id+)))
      end)))

(defun group (stream &optional end)
  "Parser for group chunk"
  (declare (ignore end))
  (let ((id (read-id stream)) (length (read-length stream))) ; group must have
                                                             ; length.
    (values
      (make-instance *group-class*
                     :id id
                     :src-path (truename stream)
                     :data (make-chunk stream length))
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

(defun compute-length (chunk)
  "Compute total size of CHUNK, every header length and padding are included."
  (etypecase chunk
    (leaf
     (+ +size-of-header+
        (ensure-even (reduce #'+ (data<-chunk chunk) :key #'length))))
    (node
     (+ +size-of-id+ (reduce #'+ (data<-chunk chunk) :key #'compute-length)))
    (group (+ +size-of-header+ (compute-length (data<-chunk chunk))))))

(defun write-length (chunk stream)
  (etypecase chunk
    ((or leaf group)
     (funcall *length-writer* (- (compute-length chunk) +size-of-header+)
              stream))
    (node #|Do nothing|#)))

(defun write-chunk (chunk stream)
  ;; ID
  (write-sequence (babel:string-to-octets (id<-chunk chunk)) stream)
  ;; LENGTH
  (write-length chunk stream)
  ;; CONTENT
  (etypecase chunk
    (leaf
     (let ((length 0))
       (dolist (vector (data<-chunk chunk))
         (incf length (length vector))
         (write-sequence vector stream))
       (when (oddp length) ; pad-needed-p
         (write-byte 0 stream))))
    (node (dolist (chunk (data<-chunk chunk)) (write-chunk chunk stream)))
    (group (write-chunk (data<-chunk chunk) stream)))
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

(defun write-iff (iff stream) (write-chunk iff stream))

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

(defun find-by-id (id chunk)
  (if (equal id (id<-chunk chunk))
      chunk
      (etypecase chunk
        (group (find-by-id id (data<-chunk chunk)))
        (node
         (find-if (lambda (data) (find-by-id id data)) (data<-chunk chunk)))
        (leaf nil))))