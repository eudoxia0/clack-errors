(in-package :cl-user)
(defpackage clack-errors
  (:use :cl :clack :clack.response)
  (:import-from :trivial-backtrace
                :print-backtrace)
  (:export :<clack-error-middleware>))
(in-package :clack-errors)

(defun slurp-file (path)
  ;; Credit: http://www.ymeme.com/slurping-a-file-common-lisp-83.html
  (with-open-file (stream path)
    (let ((seq (make-array (file-length stream) :element-type 'character :fill-pointer t)))
      (setf (fill-pointer seq) (read-sequence seq stream))
      seq)))

(defparameter *dev-css-path*
  (merge-pathnames
   #p"static/style-dev.css"
   (asdf:component-pathname (asdf:find-system :clack-errors))))

(defparameter *prod-css-path*
  (merge-pathnames
   #p"static/style-prod.css"
   (asdf:component-pathname (asdf:find-system :clack-errors))))

(defparameter *highlight-css*
  (merge-pathnames
   #p"static/highlight-lisp/themes/github.css"
   (asdf:component-pathname (asdf:find-system :clack-errors))))

(defparameter *highlight-js*
  (merge-pathnames
   #p"static/highlight-lisp/highlight-lisp.js"
   (asdf:component-pathname (asdf:find-system :clack-errors))))

(defclass <clack-error-middleware> (<middleware>)
  ((debug :type boolean
          :initarg :debug
          :accessor debug-p
          :initform t
          :documentation "If T, show the full backtrace etc.
If NIL, just a simple error page."))
  (:documentation "Middleware to catch errors."))


(defun ex-name (ex)
  (symbol-name (class-name (class-of ex))))

(defun ex-slots (ex)
  (mapcar #'closer-mop:slot-definition-name
	  (closer-mop:class-slots (class-of ex))))

(defun slot-values (obj)
  (loop for slot in (ex-slots obj)
        collecting
        (list (symbol-name slot) (slot-value obj slot))))

(defparameter +backtrace-regex+ "\\n\\w*\\d+:"
  "A regular expression to split backtraces")

(defun split-backtrace (str)
  (ppcre:split +backtrace-regex+ str))

(defun parse-backtrace (bt)
  (destructuring-bind (header &rest frames) (split-backtrace bt)
    (let ((error-msg (subseq header
                             (position #\: header :from-end t)))
          (date-time (subseq header
                             (1+ (position #\: header))
                             (position #\A header))))
      (list error-msg date-time frames))))

(defun render (bt ex env)
  (let* ((backtrace (parse-backtrace bt)))
    (dev-page:index
     (list :name (ex-name ex)
           :slots (slot-values ex)
           :datetime (nth 1 backtrace)
           :backtrace (subseq (caddr backtrace) 6)
           :url (getf env :path-info)
           :method (getf env :request-method)
           :query (getf env :query-string)
           :css (concatenate 'string
                             (slurp-file *dev-css-path*)
                             (slurp-file *highlight-css*))
           :js (slurp-file *highlight-js*)
           :env (loop for (key value) on env by #'cddr collecting
                      (list key value))))))

(defun render-prod (ex env)
  (prod-page:index (list :name (ex-name ex)
                         :url (getf env :path-info)
                         :css (slurp-file *prod-css-path*))))
 
(defmethod call ((this <clack-error-middleware>) env)
  (let ((str (make-string-output-stream)))
    (handler-case (handler-bind
                      ((t #'(lambda (condition)
                              (write-string (print-backtrace condition :output nil) str))))
                    (call-next this env))
      (t (ex) (list
               500
               '(:content-type "text/html")
               (list (if (debug-p this)
                         (render (get-output-stream-string str) ex env)
                         (render-prod ex env))))))))
