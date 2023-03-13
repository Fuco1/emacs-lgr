# lgr

lgr is a logging package for Emacs built on the back of
[EIEIO](https://www.gnu.org/software/emacs/manual/html_node/eieio/)
classes. It is designed to be flexible, performant, and
extensible.

# Features

- *Hierarchical loggers* like in log4j and python logging. This is
  useful if you want to be able to configure logging on a per-package
  basis.
- An *arbitrary number of appenders* for each logger. A single logger
  can write to the console, a logfile, a database, etc… .
- Support for structured logging. As opposed to many other logging
  packages for Emacs a log event is not just a message with a
  timestamp, but an object that can contain arbitrary data
  fields. This is useful for producing machine readable logs.
- *Lazy evaluated* arguments for log messages.  If the log event level
  is above the threshold, arguments won't be evaluated to save time.
- Appenders that write logs to a wide range of destinations:
  - minibuffer via `message`,
  - standard output with `princ`,
  - plaintext files (with a powerful formatting syntax),
  - JSON files with arbitrary data fields,
  - ... or your own custom appender.

# Usage

## How do I log!

To log an *event* with lgr, we call `(lgr-LEVEL lgr <message>)`.  Rest
of the arguments to the logging function are interpreted by `format`
until all the format control sequences are replaced, then the rest is
stored as arbitrary event metadata.

To get a `lgr` logger object, call `(lgr-get-logger "logger-name")`.
Logger name is an arbitrary string, but should somehow correspond to
your package's name.

```emacs-lisp
(lgr-fatal lgr "A critical error")
;=> [2023-03-11T01:24:49+0000] (fatal) A critical error
(lgr-error lgr "A less severe error")
;=> [2023-03-11T01:24:49+0000] (error) A less severe error
(lgr-warn lgr "A potentially bad situation")
;=> [2023-03-11T01:24:49+0000] (warn) A potentially bad situation
(lgr-info lgr "iris has %s rows" (nrow iris))
;=> [2023-03-11T01:24:49+0000] (info) iris has 150 rows

; the following log levels are hidden by default
(lgr-debug lgr "A debug message")
(lgr-trace lgr "A finer grained debug message")
```

Loggers **should never** be created manually but only be retrieved
using `lgr-get-logger`.  If a logger with the same name already
exists, it will be returned from a cache.  The common idiom is to
let-bind a logger at the beginning of a function and then use it
throughout the function.

``` emacs-lisp
(defun start-worker (worker-id)
  (let ((lgr (lgr-get-logger "package.worker")))
    (lgr-info lgr "Starting worker %d" worker-id)
    ...))

(defun main ()
  (let ((lgr (lgr-get-logger "package")))
    (lgr-info lgr "Starting the package main event loop")
    (start-worker 1)
    (start-worker 2)))
```

You can of course use multiple loggers in a single function by
let-binding multiple calls to `lgr-get-logger` (or even use them
inline).

## Give me 3 minute rundown of configuration

Logging an event by itself wont store it anywhere, for that, the
logger must be configured with an appended.  A Logger can have several
appenders to write to multiple destinations.

For example, we can add a file appender to format events as JSONs and
save them to file.  To do this, we need to configure two settings:

- add the JSON layout to the appender so it knows how to format the
  events before writing them to the file.
- attach this appender to the logger object `lgr`

Configuration is very convenient with the usage of the `->` macro from
the
[dash.el](https://github.com/magnars/dash.el#--x-optional-form-rest-more)
package, but can be equally done without.

``` emacs-lisp
(-> lgr
    (lgr-add-appender
     (-> (lgr-appender-file "json-logs.log")
         (lgr-set-layout (lgr-layout-json)))))

;; same code macro-expanded
(lgr-add-appender
 lgr
 (lgr-set-layout
  (lgr-appender-file "json-logs.log")
  (lgr-layout-json)))
```

The `->` style resembles the method "dot chaining" from traditional
OOP languages like Java or C++.  To make this possible, we make sure
that all the configuration methods always take the instance as the
first argument and return itself so they can be chained:

``` emacs-lisp
(-> lgr
    (lgr-add-appender (lgr-appender-princ))
    (lgr-set-threshold lgr-level-trace)
    (lgr-set-propagate nil))
```

## Logger hierarchies

Loggers are organized in hierarchies.  The loggers are automatically
nested by separating the segments of the name with a dot:

``` emacs-lisp
(lgr-get-logger "lgr")
(lgr-get-logger "lgr.appender")
(lgr-get-logger "lgr.layout")

;; lgr
;; ├─ appender
;; └─ layout
```

Loggers *propagate* events up the hierarchy unless configured not to
with `lgr-set-propagate`.

The most common situation is to configure appenders only on top-level
logger and let events bubble up and be processed there.  But if an
appender is added to some logger lower in the hierarchy, an event can
be dispatched twice or more times.

Use `M-x lgr-loggers-format-to-tree` to visualize the logger
hierarchy.  The results are displayed in a `*lgr loggers*` buffer:

```
lgr logger hierarchy
====================

🔇 Loggers without appenders

🔇 lgr--root [info]
├─ elsa [info] > Princ
│  └─ lsp
├─ 🔇 lgr
│  ├─ 🔇 appender
│  └─ 🔇 layout
├─ local > Warnings
│  ├─ error [error]
│  └─ test
│     ├─ one
│     └─ two
└─ 🔇 test [error]
```

## Configuring thresholds

Loggers and appenders can both be configured independently with
thresholds.

Currently, six levels are built-in in `lgr`:

- `fatal` => 100 or constant `lgr-level-fatal`
- `error` => 200 or constant `lgr-level-error`
- `warn` => 300 or constant `lgr-level-warn`
- `info` => 400 or constant `lgr-level-info` **[default]**
- `debug` => 500 or constant `lgr-level-debug`
- `trace` => 600 or constant `lgr-level-trace`

A logger won't emit an event whose level is higher than the logger
threshold.

An appender won't append an event whose level is higher than the
appender threshold.

This way, we can create interesting setups such as:

Configure one logger with two appenders, one for file logging and one
sending emails.  We configure the file appender to debug threshold and
the email appender to error threshold.

If the logger itself has an info threshold, only events info and above
will be emited.  All those will be saved in the file, because the file
appender has debug threshold.  But only fatal and error events will be
sent as emails to an SRE operator.

If a logger has no configured threshold, it will look up the logger
hierarchy to inherit the threshold of first configured logger.  This
way, you can selectively increase or decrease the log granularity of
parts of the logger hierarchy when debugging specific parts of code.

Thresholds are configured with `lgr-set-threshold` method:

``` emacs-lisp
(-> (lgr-get-logger "lgr")
    (lgr-set-threshold lgr-level-debug))
```

## Event metadata

By passing additional key-value pairs in form of a plist, you can add
arbitrary metadata to your events.

``` emacs-lisp
(lgr-info lgr "This is a message number %d" 5 :worker-id "west-eu-7" :datacenter "dc1")
```

Various layouts handle the formatting of metadata differently, you can
read in their documentation.  For example, JSON layout will serialize
it as JSON subobject under a `meta` key.

# Available Loggers, Appenders and Layouts

lgr comes with many appenders and layouts out of the box.  You can
read the built-in documentation with `C-h f <class-name>`.

Currently implemented loggers:

- `lgr-logger` - log message as-is
- `lgr-logger-format` - interpret message as format string for
  `format`, using remaining arguments as replacement.

The `lgr-logger-format` is the default format returned by
`lgr-get-logger`.

Currently implemented appenders:

- `lgr-appender` - print events using `message`
- `lgr-appender-princ` - print events using `princ` (standard output in `-batch`)
- `lgr-appender-file` - write events to a file
- `lgr-appender-buffer` - write events to a buffer
- `lgr-appender-warnings` - use `display-warning` to log events
- `lgr-appender-journald` - write logs to systemd journal

Available layouts:

- `lgr-layout-format` - use custom format string template to format events
- `lgr-layout-json` - format as JSON string

# But isn't it going to slow down my code?

No.  `lgr` uses macros to implement lazy evaluation of the arguments.
If the logger threshold doesn't exceed the event level, no arguments
to the `lgr-LEVEL` call are actually evaluated (except the logger
itself which needs to be checked).

This is why it is not advisable to use `lgr-log` directly but instead
always use the `lgr-LEVEL` macros.

# Extending lgr

The main idea of `lgr` is to make it easily extensible by adding your
own layouts and appenders.

Here is an example appender used in `lgr`'s own test suite.  It simply
pushes the events to an internal list.

``` emacs-lisp
(defclass lgr-test-appender
  ;; extend `lgr-appender' class
  (lgr-appender)
  ;; add a new slot to store the events
  ((events :type list :initform nil)))

;; implement the `log-append' method
(cl-defmethod lgr-append ((appender lgr-test-appender) (event lgr-event))
  (push event (oref appender events)))
```


Here is a more interesting example of an appender using
[emacs-async](https://github.com/jwiegley/emacs-async/pull/167) to
send messages from worker processes to the main process:

``` emacs-lisp
(defclass elsa-worker-appender (lgr-appender) ()
  "Appender sending messages back to parent process.")

(cl-defmethod lgr-append ((this elsa-worker-appender) event)
  (when async-in-child-emacs
    (async-send
     :op "echo"
     :message (lgr-format-event (oref this layout) event)))
  this)

;; configure the logging in a worker process
(-> (lgr-get-logger "elsa")
    (lgr-reset-appenders)
    (lgr-add-appender
     (-> (elsa-worker-appender)
         (lgr-set-layout (elsa-plain-layout))))
    (lgr-set-threshold lgr-level-info))
```

(as seen in [Elsa](https://github.com/emacs-elsa/Elsa))

This example shows the power of `lgr`.  We can keep the same
`lgr-info` and `lgr-debug` calls everywhere and based on the
configuration in either the main process or the worker process
different appender will be used to dispatch the messages where they
need to go.  Therefore, the logging logic, destinations and formatting
are separate from the logging *calls*.

# Using lgr in my own package.

Because all the packages loaded in Emacs share the common namespace,
there are some basic guidelines for using lgr in your own private or
published packages:

- The main logger name should correspond to your package name.
- All the loggers you use in the package should be nested under your main logger.
- If your package is used inside Emacs, you should provide some
  reasonable default configuration, for example in the major-mode
  function or as a separate function `PACKAGE-setup-lgr` that users
  can call in their init file.

That's it!.  This way, *consumers* of your package can independently
of you as the author increase or decrease or even completely disable
logging in your package.

# Acknowledgement

This library's architecture was inspired in great deal by
[s-fleck/lgr](https://github.com/s-fleck/lgr) package for R language,
which in turn is modelled after [python
logging](https://docs.python.org/3/library/logging.html).
