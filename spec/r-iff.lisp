(defpackage :r-iff.spec
  (:use :cl :jingoh :r-iff))
(in-package :r-iff.spec)
(setup :r-iff)

(requirements-about IFF :doc-type function)

;;;; Description:

#+syntax
(IFF pathname) ; => result

;;;; Arguments and Values:

; pathname := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about WRITE-IFF :doc-type function)

;;;; Description:

#+syntax
(WRITE-IFF iff stream) ; => result

;;;; Arguments and Values:

; iff := 

; stream := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about RIFF :doc-type function)

;;;; Description:

#+syntax
(RIFF pathname) ; => result

;;;; Arguments and Values:

; pathname := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about WRITE-RIFF :doc-type function)

;;;; Description:

#+syntax
(WRITE-RIFF riff stream) ; => result

;;;; Arguments and Values:

; riff := 

; stream := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about CHUNK :doc-type TYPE)

;;;; Description:
; Abstract superclass of group, node, and leaf.
;;;; Class Precedence List: (case in SBCL)
; chunk standard-object slot-object t

;;;; Effective Slots:

; ID [Type] ID
; [ACCESSOR] id<-chunk

; SRC-PATH [Type] (OR STRING PATHNAME)
; [READER] src-path<-chunk

; DATA [Type] T
; [ACCESSOR] data<-chunk

;;;; Notes:

(requirements-about LEAF :doc-type TYPE)

;;;; Description:

;;;; Class Precedence List: (case in SBCL)
; leaf chunk standard-object slot-object t

;;;; Effective Slots:

; DATA [Type] LIST

; ID [Type] ID
; [ACCESSOR] id<-chunk

; SRC-PATH [Type] (OR STRING PATHNAME)
; [READER] src-path<-chunk

;;;; Notes:

(requirements-about LEAF :doc-type function)

;;;; Description:
; Parser for leaf chunk.

#+syntax
(LEAF stream &optional end) ; => result

;;;; Arguments and Values:

; stream := stream

; end := unsigned-byte

; result 1 := chunk

; result 2 := unsigned-byte

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about NODE :doc-type TYPE)

;;;; Description:

;;;; Class Precedence List: (case in SBCL)
; node chunk standard-object slot-object t

;;;; Effective Slots:

; DATA [Type] LIST

; ID [Type] ID
; [ACCESSOR] id<-chunk

; SRC-PATH [Type] (OR STRING PATHNAME)
; [READER] src-path<-chunk

;;;; Notes:

(requirements-about NODE :doc-type function)

;;;; Description:
; Parser for node chunk.

#+syntax
(NODE stream &optional end) ; => result

;;;; Arguments and Values:

; stream := stream

; end := unsigned-byte

; result 1 := chunk

; result 2 := unsigned-byte

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about GROUP :doc-type TYPE)

;;;; Description:

;;;; Class Precedence List: (case in SBCL)
; group chunk standard-object slot-object t

;;;; Effective Slots:

; DATA [Type] NODE

; ID [Type] ID
; [ACCESSOR] id<-chunk

; SRC-PATH [Type] (OR STRING PATHNAME)
; [READER] src-path<-chunk

;;;; Notes:

(requirements-about GROUP :doc-type function)

;;;; Description:
; Parser for group chunk

#+syntax
(GROUP stream &optional end) ; => result

;;;; Arguments and Values:

; stream := stream

; end := unsigned-byte

; result 1 := chunk

; result 2 := unsigned-byte

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about ID :doc-type type)
;;;; Description:

;;;; Compound Type Specifier Kind:
; TODO: Choose one of below and delete others includes this line.
; Specializing.
; Abbreviating.
; Combining.
; Predicating.

;;;; Compound Type Specifier Syntax:

#+syntax
(id)

;;;; Compound Type Specifier Arguments:


(requirements-about ID<-CHUNK :doc-type function)

;;;; Description:

#+syntax
(ID<-CHUNK sb-pcl::object) ; => result

#+setf
(SETF (ID<-CHUNK SB-PCL::OBJECT) SB-PCL::NEW-VALUE) ; => new-value

;;;; Argument Precedence Order:
; sb-pcl::object

;;;; Method signature:
#+signature(ID<-CHUNK (CHUNK CHUNK))

;;;; Arguments and Values:

; object := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about SRC-PATH<-CHUNK :doc-type function)

;;;; Description:

#+syntax
(SRC-PATH<-CHUNK sb-pcl::object) ; => result

;;;; Argument Precedence Order:
; sb-pcl::object

;;;; Method signature:
#+signature(SRC-PATH<-CHUNK (CHUNK CHUNK))

;;;; Arguments and Values:

; object := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about DATA<-CHUNK :doc-type function)

;;;; Description:

#+syntax
(DATA<-CHUNK sb-pcl::object) ; => result

#+setf
(SETF (DATA<-CHUNK SB-PCL::OBJECT) SB-PCL::NEW-VALUE) ; => new-value

;;;; Argument Precedence Order:
; sb-pcl::object

;;;; Method signature:
#+signature(DATA<-CHUNK (CHUNK CHUNK))

;;;; Arguments and Values:

; object := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about DEFPARSER :doc-type function)

;;;; Description:

#+syntax
(DEFPARSER name parser) ; => result

;;;; Arguments and Values:

; name := 

; parser := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about *READ-DATA-ELEMENT-TYPE* :doc-type variable)

;;;; Description:

;;;; Value type is CONS
;#? *READ-DATA-ELEMENT-TYPE* :be-the ???

; Initial value is `(UNSIGNED-BYTE 8)`

;;;; Affected By:

;;;; Notes:

(requirements-about *LEAF-CLASS* :doc-type variable)

;;;; Description:

;;;; Value type is SYMBOL
;#? *LEAF-CLASS* :be-the ???

; Initial value is `LEAF`

;;;; Affected By:

;;;; Notes:

(requirements-about *NODE-CLASS* :doc-type variable)

;;;; Description:

;;;; Value type is SYMBOL
;#? *NODE-CLASS* :be-the ???

; Initial value is `NODE`

;;;; Affected By:

;;;; Notes:

(requirements-about *GROUP-CLASS* :doc-type variable)

;;;; Description:

;;;; Value type is SYMBOL
;#? *GROUP-CLASS* :be-the ???

; Initial value is `GROUP`

;;;; Affected By:

;;;; Notes:

(requirements-about +SIZE-OF-ID+ :doc-type variable)

;;;; Description:

;;;; Value type is (INTEGER 0 4611686018427387903)
;#? +SIZE-OF-ID+ :be-the ???

; Initial value is `4`

;;;; Affected By:

;;;; Notes:

(requirements-about +SIZE-OF-HEADER+ :doc-type variable)

;;;; Description:

;;;; Value type is (INTEGER 0 4611686018427387903)
;#? +SIZE-OF-HEADER+ :be-the ???

; Initial value is `8`

;;;; Affected By:

;;;; Notes:

(requirements-about MAKE-CHUNK :doc-type function)

;;;; Description:
; Top level.

#+syntax
(MAKE-CHUNK stream &optional end) ; => result

;;;; Arguments and Values:

; stream := 

; end := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about MAKE-CHUNKS :doc-type function)

;;;; Description:
; make-chunk recursively.

#+syntax
(MAKE-CHUNKS stream end &optional acc) ; => result

;;;; Arguments and Values:

; stream := 

; end := 

; acc := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about WRITE-CHUNK :doc-type function)

;;;; Description:

#+syntax
(WRITE-CHUNK chunk stream) ; => result

;;;; Arguments and Values:

; chunk := 

; stream := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about FIND-BY-ID :doc-type function)

;;;; Description:

#+syntax
(FIND-BY-ID id chunk) ; => result

;;;; Arguments and Values:

; id := 

; chunk := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about ENSURE-EVEN :doc-type function)

;;;; Description:

#+syntax
(ENSURE-EVEN integer) ; => result

;;;; Arguments and Values:

; integer := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

