(package "lgr"
         "0.1.0"
         "A fully featured logging framework")

(website-url "https://github.com/Fuco1/emacs-lgr")
(keywords "tools")

(package-file "lgr.el")

(files "lgr.el")

(script "test" "echo \"Error: no test specified\" && exit 1")

(source "gnu")
(source "melpa")

(depends-on "emacs" "26.1")

(development
 (depends-on "buttercup")
 (when (version<= "26.3" emacs-version)
   (depends-on "elsa")))
