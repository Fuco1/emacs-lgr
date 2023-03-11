(require 'buttercup)
(require 'lgr)

(describe "Logger layouts"

  (describe "lgr-layout"

    (it "should format an event using the event's lgr-to-string"
      (let ((event (lgr-event :msg "hello"
                              :level 400
                              :timestamp (list 25611 55393 715589 340000)
                              :logger-name "main"
                              :meta (list :key "value"))))
        (expect (lgr-format-event (lgr-layout) event)
                :to-equal "[2023-03-11T01:24:49+0000] (400) main - hello (:key value)"))))

  (describe "lgr-layout-json"

    (it "should format an event as a JSON string"
      (let ((event (lgr-event :msg "hello"
                              :level 400
                              :timestamp (list 25611 55393 715589 340000)
                              :logger-name "main"
                              :meta (list :key "value"))))
        (expect (lgr-format-event (lgr-layout-json) event)
                :to-equal "{\"timestamp\":\"2023-03-11T01:24:49+0000\",\"level\":400,\"logger-name\":\"main\",\"msg\":\"hello\",\"meta\":{\"key\":\"value\"}}"))))


  (describe "lgr-layout-format"

    (it "should format an event using a format string"
      (let ((event (lgr-event :msg "hello"
                              :level 400
                              :timestamp (list 25611 55393 715589 340000)
                              :logger-name "main"
                              :meta (list :key "value"))))
        (expect (lgr-format-event (lgr-layout-format :format "%t %n %g - %m %f") event)
                :to-equal "2023-03-11T01:24:49+0000 400 main - hello (:key value)")))

    (it "should have customizable timestamp-format"
      (let ((event (lgr-event :msg "hello"
                              :level 400
                              :timestamp (list 25611 55393 715589 340000)
                              :logger-name "main"
                              :meta (list :key "value"))))
        (expect (lgr-format-event (lgr-layout-format :format "%t %n %g - %m %f"
                                                     :timestamp-format "%Y-%m-%d")
                                  event)
                :to-equal "2023-03-11 400 main - hello (:key value)")))

    (describe "format spec tests"

      (it "should format %t as timestamp"
        (let ((event (lgr-event :msg "hello"
                                :level 400
                                :timestamp (list 25611 55393 715589 340000)
                                :logger-name "main"
                                :meta (list :key "value"))))
          (expect (lgr-format-event (lgr-layout-format :format "%t") event)
                  :to-equal "2023-03-11T01:24:49+0000")))

      (it "should format %l as lowercase level name"
        (let ((event (lgr-event :msg "hello"
                                :level 400
                                :timestamp (list 25611 55393 715589 340000)
                                :logger-name "main"
                                :meta (list :key "value"))))
          (expect (lgr-format-event (lgr-layout-format :format "%l") event)
                  :to-equal "info")))

      (it "should format %L as uppercase level name"
        (let ((event (lgr-event :msg "hello"
                                :level 400
                                :timestamp (list 25611 55393 715589 340000)
                                :logger-name "main"
                                :meta (list :key "value"))))
          (expect (lgr-format-event (lgr-layout-format :format "%L") event)
                  :to-equal "INFO")))

      (it "should format %k as lowercase one letter level name"
        (let ((event (lgr-event :msg "hello"
                                :level 400
                                :timestamp (list 25611 55393 715589 340000)
                                :logger-name "main"
                                :meta (list :key "value"))))
          (expect (lgr-format-event (lgr-layout-format :format "%k") event)
                  :to-equal "i")))

      (it "should format %K as uppercase one letter level name"
        (let ((event (lgr-event :msg "hello"
                                :level 400
                                :timestamp (list 25611 55393 715589 340000)
                                :logger-name "main"
                                :meta (list :key "value"))))
          (expect (lgr-format-event (lgr-layout-format :format "%K") event)
                  :to-equal "I")))

      (it "should format %n as numeric level"
        (let ((event (lgr-event :msg "hello"
                                :level 400
                                :timestamp (list 25611 55393 715589 340000)
                                :logger-name "main"
                                :meta (list :key "value"))))
          (expect (lgr-format-event (lgr-layout-format :format "%n") event)
                  :to-equal "400")))

      (it "should format %g as logger name"
        (let ((event (lgr-event :msg "hello"
                                :level 400
                                :timestamp (list 25611 55393 715589 340000)
                                :logger-name "main"
                                :meta (list :key "value"))))
          (expect (lgr-format-event (lgr-layout-format :format "%g") event)
                  :to-equal "main")))

      (it "should format %m as message"
        (let ((event (lgr-event :msg "hello"
                                :level 400
                                :timestamp (list 25611 55393 715589 340000)
                                :logger-name "main"
                                :meta (list :key "value"))))
          (expect (lgr-format-event (lgr-layout-format :format "%m") event)
                  :to-equal "hello")))

      (it "should format %f as plist representation of meta"
        (let ((event (lgr-event :msg "hello"
                                :level 400
                                :timestamp (list 25611 55393 715589 340000)
                                :logger-name "main"
                                :meta (list :key "value"))))
          (expect (lgr-format-event (lgr-layout-format :format "%f") event)
                  :to-equal "(:key value)")))

      (it "should format %j as JSON representation of meta"
        (let ((event (lgr-event :msg "hello"
                                :level 400
                                :timestamp (list 25611 55393 715589 340000)
                                :logger-name "main"
                                :meta (list :key "value"))))
          (expect (lgr-format-event (lgr-layout-format :format "%j") event)
                  :to-equal "{\"key\":\"value\"}"))))))
