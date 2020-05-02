; vim: ft=lisp et
(in-package :asdf)
(defsystem "r-iff.test"
  :version
  "0.3.8"
  :depends-on
  (
   :jingoh
    "r-iff"
    "flexi-streams" ; in-memory stream.
    )
  :components
  ((:file "r-iff"))
  :perform
  (test-op (o c) (declare (special args))
   (apply #'symbol-call :jingoh :examine :r-iff args)))
