(defpackage :r-iff.spec
  (:use :cl :jingoh :r-iff)
  (:import-from :r-iff #:read-data #:read-id #:read-length))
(in-package :r-iff.spec)
(setup :r-iff)

(common-requirements-about (IFF RIFF) :as op :doc-type function)

;;;; Description:

#+syntax
(op pathname) ; => result

;;;; Arguments and Values:

; pathname := pathname-designator, otherwise signals conditioin.
#?(op '#:not-pathname-designator) :signals condition

; result := chunk

;;;; Affected By:
; `*GROUP-CLASS*`, see `GROUP`.
; `*NODE-CLASS*`, see `NODE`.
; `*LEAF-CLASS*`, see `LEAF`.
; `*DATA-SIZE-LIMIT*`, see `READ-DATA`.
; `*READ-DATA-ELEMENT-TYPE*`, see `READ-DATA`.

;;;; Side-Effects:
; Access file system.

;;;; Notes:

;;;; Exceptional-Situations:
; If file does not have enough length, end-of-file is signaled.

(requirements-about WRITE-IFF :doc-type function)

;;;; Description:

#+syntax
(WRITE-IFF iff stream) ; => result

;;;; Arguments and Values:

; iff := chunk, otherwise signals condition.
#?(write-iff "Not chunk" *standard-output*) :signals condition

; stream := Stream, otherwise signals condition.
#?(write-iff (make-instance 'group :id "tree") "Not stream") :signals condition

; result := iff
#?(write-iff (make-instance 'group :id "test" :data nil) (make-broadcast-stream))
:be-the chunk

;;;; Affected By:

;;;; Side-Effects:
; Output to stream.

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

; SRC-PATH [Type] (OR STRING PATHNAME NULL)
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

; stream := stream otherwise signals condition.
#?(leaf "Not stream") :signals condition

; end := unsigned-byte, Ignored.

; result 1 := chunk
#?(flex:with-input-from-sequence (in (concatenate 'vector
                                                  (babel:string-to-octets "test")
                                                  #(0 0 0 0)))
    (leaf in))
:be-the chunk

#?(flex:with-input-from-sequence (in (concatenate 'vector
                                                  (babel:string-to-octets "test")
                                                  #(0 0 0 2 3 4)))
    (leaf in))
:multiple-value-satisfies
(lambda (chunk length)
  (& (typep chunk 'chunk)
     (equal "test" (id<-chunk chunk))
     (= 10 (r-iff::compute-length chunk))
     (equalp '(#(3 4)) (data<-chunk chunk))
     (= 10 length)))

; result 2 := unsigned-byte, total size of leaf.
#?(flex:with-input-from-sequence (in (concatenate 'vector
                                                  (babel:string-to-octets "test")
                                                  #(0 0 0 0)))
    (nth-value 1 (leaf in)))
=> 8

;;;; Affected By:
; `*LEAF-CLASS*`

; `R-IFF::*DATA-SIZE-LIMIT*`
#?(flex:with-input-from-sequence (in (concatenate 'vector
                                                  (babel:string-to-octets "test")
                                                  #(0 0 0 2 3 4)))
    (let ((r-iff::*data-size-limit* 1))
      (leaf in)))
:multiple-value-satisfies
(lambda (chunk length)
  (& (typep chunk 'chunk)
     (equal "test" (id<-chunk chunk))
     (= 10 (r-iff::compute-length chunk))
     (equalp '(#(3) #(4)) (data<-chunk chunk))
     (= 10 length)))

;;;; Side-Effects:
; Consume stream.
#?(flex:with-input-from-sequence (in (concatenate 'vector
                                                  (babel:string-to-octets "test")
                                                  #(0 0 0 0 1)))
    (values (leaf in) (read-byte in)))
:multiple-value-satisfies
(lambda (chunk byte)
  (& (typep chunk 'chunk)
     (= 1 byte)))

;;;; Notes:

;;;; Exceptional-Situations:
; When stream has less than 8 byte, end-of-file is signaled.
#?(flex:with-input-from-sequence (in (concatenate 'vector
                                                  (babel:string-to-octets "test")
                                                  #(0 0 0)))
    (leaf in))
:signals end-of-file

; When stream does not have specified length of bytes, end-of-file is signaled.
#?(flex:with-input-from-sequence (in (concatenate 'vector
                                                  (babel:string-to-octets "test")
                                                  #(0 0 0 1)))
    (leaf in))
:signals end-of-file

;;;; ADDITIONAL TEST:
; When size is odd, one more byte (padd) is consumed.
#?(flex:with-input-from-sequence (in (concatenate 'vector
                                                  (babel:string-to-octets "test")
                                                  #(0 0 0 1) ; <--- Specify body size.
                                                  #(2) ; <--- The body.
                                                  #(3) ; <--- Padding.
                                                  #(4))) ; <--- As sentinel.
    (values (leaf in) (read-byte in)))
:multiple-value-satisfies
(lambda (leaf byte)
  (& (typep leaf 'leaf)
     (equal "test" (id<-chunk leaf))
     (= 9 (r-iff::compute-length leaf))
     (equalp '(#(2)) (data<-chunk leaf))
     (eql 4 byte)))

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

; stream := stream, otherwise condition.
#?(node "Not stream") :signals condition

; end := (integer 4 #xFFFFFFFF), otherwise condition.
#?(node *standard-input* "Not integer") :signals condition
#?(node *standard-input* -1) :signals condition
#?(node *standard-input* (1+ #xFFFFFFFF)) :signals condition

; result 1 := node
#?(flex:with-input-from-sequence (in (babel:string-to-octets "test"))
    (node in 4))
:be-the node

; result 2 := (integer 4 #xFFFFFFFF), represents total length of the node.
#?(flex:with-input-from-sequence (in (babel:string-to-octets "test"))
    (nth-value 1 (node in 4)))
=> 4

;;;; Affected By:
; `*NODE-CLASS*`

;;;; Side-Effects:
; Consume stream.
#?(flex:with-input-from-sequence (in (concatenate 'vector
                                                  (babel:string-to-octets "test")
                                                  #(0)))
    (values (node in 4) (read-byte in)))
:multiple-value-satisfies
(lambda (node byte)
  (& (typep node 'node)
     (equal "test" (id<-chunk node))
     (eql 0 byte)))

;;;; Notes:

;;;; Exceptional-Situations:
; When stream less than 4 bytes, end-of-file is signaled.
#?(flex:with-input-from-sequence (in (babel:string-to-octets "tes"))
    (node in 4))
:signals end-of-file

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

; stream := stream, otherwise signals condition.
#?(group "Not stream") :signals condition

; end := ignored.

; result 1 := group
#?(flex:with-input-from-sequence (in (concatenate 'vector (babel:string-to-octets "test")
                                                  #(0 0 0 0)))
    (group in))
:be-the group

; result 2 := (integer 4 #xFFFFFFFF)
#?(flex:with-input-from-sequence (in (concatenate 'vector (babel:string-to-octets "test")
                                                  #(0 0 0 0)))
    (nth-value 1 (group in)))
=> 8

;;;; Affected By:
; `*GROUP-CLASS*`

;;;; Side-Effects:
; Consume stream.
#?(flex:with-input-from-sequence (in (concatenate 'vector (babel:string-to-octets "test")
                                                  #(0 0 0 0)
                                                  #(1)))
    (values (group in) (read-byte in)))
:multiple-value-satisfies
(lambda (group byte)
  (& (typep group 'group)
     (equal "test" (id<-chunk group))
     (= 8 (r-iff::compute-length group))
     (eql 1 byte)))

;;;; Notes:

;;;; Exceptional-Situations:
; When stream does not have enough length of bytes, end-of-file is signaled.
#?(flex:with-input-from-sequence (in (babel:string-to-octets "test"))
    (group in))
:signals end-of-file

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

; chunk := chunk otherwise condition.
#?(write-chunk "Not chunk" *standard-output*) :signals condition

; stream := stream, otherwise condition.
#?(write-chunk (make-instance 'node :id "dumy") "Not stream") :signals condition

; result := CHUNK

;;;; Affected By:

;;;; Side-Effects:
; Outputs to stream.

;;;; Notes:

;;;; Exceptional-Situations:

;;;; Examples:
#?(flex:with-output-to-sequence (out)
    (flex:with-input-from-sequence (in (concatenate 'vector
                                                    (babel:string-to-octets "test")
                                                    #(0 0 0 0)))
      (write-chunk (leaf in) out)))
=> #(116 101 115 116 0 0 0 0)
,:test equalp

#?(flex:with-output-to-sequence (out)
    (flex:with-input-from-sequence (in (concatenate 'vector
                                                    (babel:string-to-octets "test")
                                                    #(0 0 0 2) ; <-- Specify body is 2 bytes.
                                                    #(3 4) ; <-- Body.
                                                    #(5))) ; <-- Never included.
      (write-chunk (leaf in) out)))
=> #(116 101 115 116 0 0 0 2 3 4)
,:test equalp

; When leaf body has odd length size, padding are written.
#?(flex:with-output-to-sequence (out)
    (flex:with-input-from-sequence (in (concatenate 'vector
                                                    (babel:string-to-octets "test")
                                                    #(0 0 0 1) ; <--- Specify size.
                                                    #(2) ; <--- The body.
                                                    #(3) ; <--- Padding.
                                                    #(4))) ; <--- Never included.
      (write-chunk (leaf in) out)))
=> #(116 101 115 116 0 0 0 1 2 0)
,:test equalp

#?(flex:with-output-to-sequence (out)
    (flex:with-input-from-sequence (in (babel:string-to-octets "test"))
      (write-chunk (node in 4) out)))
=> #(116 101 115 116)
,:test equalp

#?(flex:with-output-to-sequence (out)
    (flex:with-input-from-sequence (in (concatenate 'vector
                                                    (babel:string-to-octets "node")
                                                    (babel:string-to-octets "leaf")
                                                    #(0 0 0 2) ; <--- Specify body is 2 bytes.
                                                    #(3 4) ; <--- Body.
                                                    #(5))) ; <--- Never included.
      (write-chunk (node in 14) out)))
=> #(110 111 100 101
     108 101 97 102
     0 0 0 2
     3 4)
,:test equalp
,:ignore-signals warning

#?(flex:with-output-to-sequence (out)
    (flex:with-input-from-sequence (in (concatenate 'vector
                                                    (babel:string-to-octets "test")
                                                    #(0 0 0 0)))
      (write-chunk (group in) out)))
=> #(116 101 115 116 0 0 0 0)
,:test equalp

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

(requirements-about READ-DATA :doc-type function)

;;;; Description:

#+syntax
(READ-DATA stream size) ; => result

;;;; Arguments and Values:

; stream := input-stream, otherwise condition.
#?(read-data "Not stream" 0) :signals condition

; size := (integer 0 *), otherwise condition
#?(read-data *standard-input* "Not integer") :signals condition
#?(read-data *standard-input* -1) :signals condition

; result := (list vector)

;;;; Affected By:
; `*READ-DATA-ELEMENT-TYPE*`

;;;; Side-Effects:
; Consume stream.

;;;; Notes:

;;;; Exceptional-Situations:
; When size over stream, end-of-file is signaled.
#?(with-input-from-string (s "test")
    (let ((*read-data-element-type* 'character))
      (read-data s 5)))
:signals end-of-file

;;;; Examples:
#?(with-input-from-string (s "test")
    (let ((*read-data-element-type* 'character))
      (read-data s 4)))
=> ("test")
,:test equalp

#?(with-input-from-string (s "test")
    (let ((*read-data-element-type* 'character))
      (read-data s 3)))
=> ("tes")
,:test equalp

(requirements-about READ-ID :doc-type function)

;;;; Description:

#+syntax
(READ-ID stream) ; => result

;;;; Arguments and Values:

; stream := input stream otherwise error.
#?(read-id "Not stream") :signals condition

; result := id
#?(flex:with-input-from-sequence (in (babel:string-to-octets "test"))
    (read-id in))
=> "test"
,:test equal

;;;; Affected By:

;;;; Side-Effects:
; Consume stream.

;;;; Notes:

;;;; Exceptional-Situations:
; If stream has less than 4 bytes, end-of-file is signaled.
#?(flex:with-input-from-sequence (in (babel:string-to-octets "les"))
    (read-id in))
:signals end-of-file

#?(flex:with-input-from-sequence (in (babel:string-to-octets "12345"))
    (list (read-id in)
          (code-char (read-byte in))))
=> ("1234" #\5)
,:test equal

(requirements-about READ-LENGTH :doc-type function)

;;;; Description:

#+syntax
(READ-LENGTH stream) ; => result

;;;; Arguments and Values:

; stream := Input stream otherwise signals condition.
#?(read-length "Not stream") :signals condition

; result := (unsigned-byte 32)
#?(flex:with-input-from-sequence (in #(1 2 3 4))
    (read-length in))
=> #B00000001000000100000001100000100

;;;; Affected By:
; `r-iff::*LENGTH-READER*`
#?(let((r-iff::*length-reader* 'nibbles:read-ub32/le))
    (flex:with-input-from-sequence (in #(1 2 3 4))
      (read-length in)))
=> #B00000100000000110000001000000001

;;;; Side-Effects:
; Consume stream.

#?(flex:with-input-from-sequence (in #(1 2 3 4 5))
    (list (read-length in) (read-byte in)))
=> (#B00000001000000100000001100000100 5)
,:test equal

;;;; Notes:

;;;; Exceptional-Situations:
; When stream has less than 4 bytes, end-of-file is signaled.
#?(flex:with-input-from-sequence (in #(1 2 3))
    (read-length in))
:signals end-of-file
