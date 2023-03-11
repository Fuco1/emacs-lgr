(require 'buttercup)
(require 'lgr)

(load "tests/helpers.el")

(describe "Lgr logger"

  (before-each
    (setq lgr--loggers
          (let ((ht (make-hash-table :test #'equal)))
            (puthash "lgr--root" (lgr-logger :name "lgr--root" :threshold 400) ht)
            ht)))

  (describe "Propagation of events"

    (it "should propagate event to parent loggers"
      (let* ((appender (lgr-test-appender))
             (logger (lgr-add-appender (lgr-get-logger "test") appender))
             (sublogger (lgr-add-appender (lgr-get-logger "test.sub") appender)))
        (lgr-log sublogger 100 "test")
        (expect (length (oref appender events)) :to-equal 2)))

    (it "should not propagate to parents if propagate is set to nil"
      (let* ((appender (lgr-test-appender))
             (logger (lgr-add-appender (lgr-get-logger "test") appender))
             (sublogger (lgr-set-propagate
                         (lgr-add-appender (lgr-get-logger "test.sub") appender)
                         nil)))
        (lgr-log sublogger 100 "test")
        (expect (length (oref appender events)) :to-equal 1))))

  (describe "Checking event level against threshold"

    (it "should not create an event if level of parent is less than threshold"
      (let ((logger (lgr-add-appender (lgr-get-logger "test") (lgr-test-appender))))
        (lgr-log logger 500 "This should not be logged")
        (expect (oref (car (oref logger appenders)) events) :to-be nil)))

    (it "should create an event if level of parent is greater than threshold"
      (let ((logger (lgr-add-appender (lgr-get-logger "test") (lgr-test-appender))))
        (lgr-log logger 100 "This should be logged")
        (expect (oref (car (oref logger appenders)) events) :not :to-be nil)))

    (it "should not create an event if level on this logger is less than threshold"
      (let ((logger (lgr-set-threshold
                     (lgr-add-appender
                      (lgr-get-logger "test")
                      (lgr-test-appender))
                     300)))
        (lgr-log logger 500 "This should not be logged")
        (expect (oref (car (oref logger appenders)) events) :to-be nil)))

    (it "should create an event if level on this logger is greater than threshold"
      (let ((logger (lgr-set-threshold
                     (lgr-add-appender
                      (lgr-get-logger "test")
                      (lgr-test-appender))
                     300)))
        (lgr-log logger 100 "This should be logged")
        (expect (oref (car (oref logger appenders)) events) :not :to-be nil))))

  (describe "Checking appender level before appending"

    (it "should not append if level of event is more than appender threshold"
      (let ((logger (lgr-add-appender
                     (lgr-get-logger "test")
                     (lgr-test-appender :threshold 300))))
        (lgr-log logger 500 "This should not be logged")
        (expect (oref (car (oref logger appenders)) events) :to-be nil)))

    (it "should append if level of event is less than appender threshold"
      (let ((logger (lgr-add-appender
                     (lgr-get-logger "test")
                     (lgr-test-appender :threshold 300))))
        (lgr-log logger 100 "This should be logged")
        (expect (oref (car (oref logger appenders)) events) :not :to-be nil)))))
