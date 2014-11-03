(cl:in-package :cl-user)

(defpackage #:cl-case-control-asd
  (:use :cl :asdf) )

(in-package #:cl-case-control-asd)

(defsystem cl-case-control
  :name "cl-case-control"
  :version "0.1.0"
  :maintainer "SUZUKI Shingo"
  :author "SUZUKI Shingo"
  :licence "MIT"
  :description "Supporting case-controlling"
  :depends-on (:trivial-types)
  :components ((:file "case-control")) )

#| ; for future work
(defmethod perform ((op test-op) (compo (eql (find-system :cl-case-control))))
  (declare (ignore op compo))
  (operate 'load-op :cl-case-control-test)
  (operate 'test-op :cl-case-control-test :force t) )
|#


