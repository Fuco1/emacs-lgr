(require 'eieio)

(defclass lgr-test-appender (lgr-appender)
  ((events :initform nil)))

(cl-defmethod lgr-append ((appender lgr-test-appender) event)
  (push event (oref appender events)))
