(require 'buttercup)
(require 'lgr)

(describe "Appender test"

  (describe "lgr-appender"

    (it "should set threshold"
      (let ((appender (lgr-appender)))
        (expect (lgr-get-threshold (lgr-set-threshold appender lgr-level-debug))
                :to-equal lgr-level-debug)))

    (it "should set layout"
      (let ((appender (lgr-appender)))
        (expect (lgr-to-string
                 (lgr-get-layout (lgr-set-layout appender (lgr-layout-json))))
                :to-equal "json layout")))

    (it "should return itself from lgr-append"
      (let ((appender (lgr-appender)))
        (expect (lgr-append appender (lgr-event :msg ""
                                                :logger-name ""
                                                :timestamp (current-time)
                                                :level 400))
                :to-equal appender)))))
