;;; lgr.el --- A fully featured logging framework for Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2023 Matúš Goljer

;; Author: Matúš Goljer <matus.goljer@gmail.com>
;; Maintainer: Matúš Goljer <matus.goljer@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1"))
;; Created: 10th March 2023
;; Keywords: logging

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'seq)
(require 'eieio)
(require 'format-spec)

(eval-and-compile
  (defconst lgr-log-levels '(
                             (fatal . 100)
                             (error . 200)
                             (warn . 300)
                             (info . 400)
                             (debug . 500)
                             (trace . 600)
                             )
    "Log levels used in `lgr-log'.")

  (defmacro lgr--def-level (level)
    `(defconst ,(intern (concat "lgr-level-" (symbol-name level)))
       ,(cdr (assq level lgr-log-levels))))

  (lgr--def-level fatal)
  (lgr--def-level error)
  (lgr--def-level warn)
  (lgr--def-level info)
  (lgr--def-level debug)
  (lgr--def-level trace))

;; (lgr-level-to-name :: (function (int) string))
(defun lgr-level-to-name (level)
  (cdr (assq
        level
        (mapcar
         (lambda (x) (cons (cdr x) (car x)))
         lgr-log-levels))))

;; (lgr-to-string :: (function (mixed) string))
(cl-defgeneric lgr-to-string (obj)
  "Format OBJ as string."
  (format "%s" obj))

(defclass lgr-event ()
  ((msg
    :type string
    :initarg :msg
    :documentation "String message to be appended.")
   (level
    :type integer
    :initarg :level
    :documentation "Log level.  See `lgr-log-levels'")
   (timestamp
    :type (list-of integer)
    :initarg :timestamp
    :documentation "Timestamp created by `current-time'.")
   (logger-name
    :type string
    :initarg :logger-name
    :documentation "Name of the logger which emited this event.")
   (meta
    :type list
    :initarg :meta
    :initform nil
    :documentation "Plist of arbitrary additional metadata."))
  "Log event produced by `lgr-logger'.

To provide different way of formatting, rather than extending the
event and providing different `lgr-to-string', it is advisable to
extend `lgr-layout' and implement new `lgr-format-event' method.

The event can be extended along with a logger if we want to track
additional properties.")

(cl-defmethod lgr-to-string ((this lgr-event))
  (format "[%s] (%s) %s - %s%s"
          (format-time-string "%FT%T%z" (oref this timestamp))
          (oref this level)
          (oref this logger-name)
          (oref this msg)
          (if (oref this meta)
              (format " %s" (oref this meta))
            "")))

(defclass lgr-layout () ()
  "Layouts format `lgr-event' as strings for appending via `lgr-appender'.

A layout needs to implement the method `lgr-format-event'.")

;; (lgr-format-event :: (function ((class lgr-layout) (class lgr-event)) string))
(cl-defgeneric lgr-format-event (this event)
  "Format `lgr-event' EVENT to string.")

(cl-defmethod lgr-format-event ((_this lgr-layout) (event lgr-event))
  "Default implementation calls `lgr-to-string' on the passed EVENT."
  (lgr-to-string event))

(defclass lgr-layout-format (lgr-layout)
  ((format
    :type string
    :initarg :format
    :documentation "Format spec.")
   (timestamp-format
    :type string
    :initarg :timestamp-format
    :initform "%FT%T%z"
    :documentation "Time format string used by `format-time-string'."))
  "Format the event using a format spec.

%t The timestamp of the message, formatted according to
   timestamp-format.
%l The log level, lowercase character representation.
%L The log level, uppercase character representation.
%k The log level, first letter of lowercase character
   representation.
%K The log level, first letter of uppercase character
   representation.
%n The log level, integer representation.
%g The name of the logger.
%m The log message.
%f All custom fields of x as a plist.
%j All custom fields of x in proper JSON.")

(cl-defmethod lgr-format-event ((this lgr-layout-format) (event lgr-event))
  (format-spec
   (oref this format)
   (format-spec-make
    ?t (format-time-string (oref this timestamp-format) (oref event timestamp))
    ?l (downcase (symbol-name (lgr-level-to-name (oref event level))))
    ?L (upcase (symbol-name (lgr-level-to-name (oref event level))))
    ?k (substring (downcase (symbol-name (lgr-level-to-name (oref event level)))) 0 1)
    ?K (substring (upcase (symbol-name (lgr-level-to-name (oref event level)))) 0 1)
    ?n (oref event level)
    ?g (oref event logger-name)
    ?m (oref event msg)
    ?f (oref event meta)
    ?j (json-serialize (oref event meta)))))

(defclass lgr-layout-json (lgr-layout) ()
  "Format the event as JSON.")

(cl-defmethod lgr-to-string ((_this lgr-layout-json))
  "json layout")

(cl-defmethod lgr-format-event ((_this lgr-layout-json) (event lgr-event))
  (json-serialize
   (list
    :timestamp (format-time-string "%FT%T%z" (oref event timestamp))
    :level (oref event level)
    :logger-name (oref event logger-name)
    :msg (oref event msg)
    :meta (oref event meta))))

;; (lgr-set-threshold :: (function (mixed int) mixed))
(cl-defgeneric lgr-set-threshold (this level)
  "Set threshold for THIS to LEVEL.

The implementations should always return THIS.")

;; (lgr-set-threshold :: (function (mixed) int))
(cl-defgeneric lgr-get-threshold (this)
  "Get threshold for THIS.")

(defclass lgr-appender ()
  ((threshold
    :type integer
    :initarg :threshold
    :initform (progn lgr-level-trace)
    :accessor lgr-get-threshold)
   (layout
    :type lgr-layout
    :initarg :layout
    :initform (progn (lgr-layout))
    :accessor lgr-get-layout))
  "Appender manages the output of events to proper destinations.

Destination can be the terminal, a file, an external service, en
Elisp function or anything else in general.

Appenders only handle the *destination* where the event is
output.

Appenders can have thresholds configured independently of
loggers.  For example, one logger can have one file appenders
logging everything and one email appender for errors only.  This
way, all events can be logged to the file but only error and
above will be emailed to a SRE personnel.

To change the way events are formatted, change the layout for
this appender with `lgr-set-layout'.  See `lgr-layout'.")

;; (lgr-append :: (function ((class lgr-appender) (class lgr-layout)) (class lgr-appender)))
(cl-defgeneric lgr-set-layout (this layout)
  "Set the LAYOUT for THIS appender.")

;; (lgr-append :: (function ((class lgr-appender) (class lgr-event)) (class lgr-appender)))
(cl-defgeneric lgr-append (this event)
  "Append EVENT to THIS appender's output.")

(cl-defmethod lgr-to-string ((_this lgr-appender))
  "message appender")

(cl-defmethod lgr-set-threshold ((this lgr-appender) level)
  (oset this threshold level)
  this)

(cl-defmethod lgr-set-layout ((this lgr-appender) (layout lgr-layout))
  (oset this layout layout)
  this)

(cl-defmethod lgr-append ((this lgr-appender) (event lgr-event))
  "Print the EVENT to minibuffer using `message'."
  (message "%s" (lgr-format-event (oref this layout) event))
  this)

(defclass lgr-appender-princ (lgr-appender) ()
  "Print events to standard output using `princ'.")

(cl-defmethod lgr-append ((this lgr-appender-princ) event)
  "Print the EVENT to standard output using `princ'."
  (princ (lgr-format-event (oref this layout) event))
  (princ "\n")
  this)

(defclass lgr-appender-file (lgr-appender)
  ((file
    :type string
    :initarg :file
    :documentation "Destination file."))
  "Log events to a file.")

(cl-defmethod lgr-append ((this lgr-appender-file) event)
  "Print the EVENT to a file."
  (let ((msg (lgr-format-event (oref this layout) event)))
    (with-temp-buffer
      (insert msg)
      (append-to-file (point-min) (point-max) (oref this file))))
  this)

(defclass lgr-logger ()
  ((name
    :type string
    :initarg :name
    :documentation "Name of the logger.")
   (threshold
    :type integer
    :initarg :threshold
    :documentation "Threshold for this logger.")
   (propagate
    :type boolean
    :initarg :propagate
    :initform t
    :documentation "If non-nil, propagate events to parent loggers.")
   (parent
    :type (or lgr-logger null)
    :initarg :parent
    :initform nil
    :documentation "Parent logger.")
   (appenders
    :type (list-of lgr-appender)
    :initform nil
    :documentation "List of appenders for this logger."))
  "Logger produces `lgr-event' objects and passes them to appenders.")

(cl-defmethod lgr-set-threshold ((this lgr-logger) level)
  (oset this threshold level)
  this)

(cl-defmethod lgr-get-threshold ((this lgr-logger))
  "Get the threshold of this appender or first configured parent.

If this logger has no configured threshold, recursively check the
parents and pick the first one with configured level.

The root logger which is always present has default level info."
  (if (slot-boundp this 'threshold)
      (oref this threshold)
    (when-let ((parent (oref this parent)))
      (lgr-get-threshold parent))))

;; (lgr-set-propagate :: (function ((class lgr-logger) bool) (class lgr-logger)))
(cl-defgeneric lgr-set-propagate (this propagate)
  "Set PROPAGATE for THIS."
  (oset this propagate propagate)
  this)

;; (lgr-add-appender :: (function ((class lgr-logger) (class lgr-appender)) (class lgr-logger)))
(cl-defgeneric lgr-add-appender (this appender)
  (oset this appenders
        (append (oref this appenders) (list appender)))
  this)

;; (lgr-remove-appender :: (function ((class lgr-logger) int) (class lgr-logger)))
(cl-defgeneric lgr-remove-appender (this pos)
  (oset this appenders
        (append (seq-take (oref this appenders) (1- pos))
                (seq-drop (oref this appenders) pos)))
  this)

;; (lgr-remove-appender :: (function ((class lgr-logger)) (class lgr-logger)))
(cl-defgeneric lgr-reset-appenders (this)
  "Remove all appenders from THIS logger."
  (oset this appenders nil)
  this)

;; (lgr-get-appenders :: (function ((class lgr-logger)) (list (class lgr-appender))))
(cl-defgeneric lgr-get-appenders (this)
  (append (oref this appenders)
          (when-let ((parent (oref this parent)))
            (when (oref this propagate)
              (lgr-get-appenders parent)))))

;; (lgr-log :: (function ((class lgr-logger) int string &rest mixed) mixed))
(cl-defgeneric lgr-log (this level message &rest meta)
  "Log a MESSAGE at a specific LEVEL.

Rather than using this function directly it is preferable to use
the logging macros which implement lazy evaluation for the
arguments.  In case the logger level is less than the granularity
of the event to be produced, the arguments will not be
evaluated.

The logging macros are:

- `lgr-fatal'
- `lgr-error'
- `lgr-warn'
- `lgr-info'
- `lgr-debug'
- `lgr-trace'."
  (when (>= (lgr-get-threshold this) level)
    (let ((event (lgr-event :msg message
                            :level level
                            :timestamp (current-time)
                            :logger-name (oref this name)
                            :meta meta)))
      (dolist (app (oref this appenders))
        (when (>= (oref app threshold) (oref event level))
          (lgr-append app event))))))

(defmacro lgr-fatal (this message &rest meta)
  "Log MESSAGE at fatal level."
  (macroexp-let2 symbolp logger this
    `(when (>= (lgr-get-threshold ,logger) lgr-level-error)
       (apply #'lgr-log ,logger lgr-level-error ,message (list ,@meta)))))

(defmacro lgr-error (this message &rest meta)
  "Log MESSAGE at error level."
  (macroexp-let2 symbolp logger this
    `(when (>= (lgr-get-threshold ,this) lgr-level-error)
       (apply #'lgr-log ,this lgr-level-error ,message (list ,@meta)))))

(defmacro lgr-warn (this message &rest meta)
  "Log MESSAGE at warn level."
  (macroexp-let2 symbolp logger this
    `(when (>= (lgr-get-threshold ,this) lgr-level-warn)
       (apply #'lgr-log ,this lgr-level-warn ,message (list ,@meta)))))

(defmacro lgr-info (this message &rest meta)
  "Log MESSAGE at info level."
  (macroexp-let2 symbolp logger this
    `(when (>= (lgr-get-threshold ,this) lgr-level-info)
       (apply #'lgr-log ,this lgr-level-info ,message (list ,@meta)))))

(defmacro lgr-debug (this message &rest meta)
  "Log MESSAGE at debug level."
  (macroexp-let2 symbolp logger this
    `(when (>= (lgr-get-threshold ,this) lgr-level-debug)
       (apply #'lgr-log ,this lgr-level-debug ,message (list ,@meta)))))

(defmacro lgr-trace (this message &rest meta)
  "Log MESSAGE at trace level."
  (macroexp-let2 symbolp logger this
    `(when (>= (lgr-get-threshold ,this) lgr-level-trace)
       (apply #'lgr-log ,this lgr-level-trace ,message (list ,@meta)))))

(defvar lgr--loggers (let ((ht (make-hash-table :test #'equal)))
                       (puthash "lgr--root" (lgr-logger :name "lgr--root" :threshold 400) ht)
                       ht)
  "Hash table of configured loggers.")

;; (lgr-get-all-loggers :: (function () (list (class lgr-logger))))
(defun lgr-get-all-loggers ()
  "Return a list of all configured loggers."
  (hash-table-values lgr--loggers))

(defun lgr-get-logger (name &optional logger-class)
  "Return a logger with NAME.

If no logger with NAME exists, create a new one and return it.

The NAME is dot-separated hierarchy of loggers.  For example, the
name \"foo.bar\" will create a logger named \"bar\" with parent
\"foo\".

Each logger has automatically a root logger as parent.  The root
logger is named \"lgr--root\" and has a threshold of 400.  This
logger should not be used directly.

You should always configure at least one top-level logger for
your package named after your package.  This way consumers can
selectively enable or disable logging for your package.

If LOGGER-CLASS is non-nil, use it to create the logger of that
class."
  (setq logger-class (or logger-class 'lgr-logger-format))
  (if-let ((logger (gethash name lgr--loggers)))
      logger
    (let* ((path (split-string name "\\."))
           (parent (if (= (length path) 1)
                       (gethash "lgr--root" lgr--loggers)
                     (lgr-get-logger (string-join (seq-take path (1- (length path))) ".") logger-class)))
           (logger (funcall logger-class :name name :parent parent)))
      (puthash name logger lgr--loggers)
      logger)))

(defclass lgr-logger-format (lgr-logger) ()
  "Logger which uses `format' to format messages.")

;; (lgr--count-format-sequences :: (function (string) int))
(defun lgr--count-format-sequences (string)
  "Count the number of format sequences in STRING."
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (let ((count 0))
      (while (re-search-forward "%" nil t)
        (if (looking-at-p "%")
            (forward-char 1)
          (cl-incf count)))
      count)))

(cl-defmethod lgr-log ((this lgr-logger-format) level message &rest meta)
  "Log a MESSAGE at a specific LEVEL using a format string.

The MESSAGE is a format string and the META is a list of
arguments to be passed to `format' followed by optional
key-value pairs (as plist) which is stored in the event
as metadata."
  (when (>= (lgr-get-threshold this) level)
    (let* ((num-of-format-controls (lgr--count-format-sequences message))
           (event (lgr-event :msg (apply #'format message (seq-take meta num-of-format-controls))
                             :level level
                             :timestamp (current-time)
                             :logger-name (oref this name)
                             :meta (seq-drop meta num-of-format-controls))))
      (dolist (app (lgr-get-appenders this))
        (when (>= (lgr-get-threshold app) (oref event level))
          (lgr-append app event))))))

(provide 'lgr)
;;; lgr.el ends here
