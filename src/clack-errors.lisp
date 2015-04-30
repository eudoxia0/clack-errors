(in-package :cl-user)
(defpackage clack-errors
  (:use :cl)
  (:import-from :trivial-backtrace
                :print-backtrace)
  (:export :*clack-error-middleware*))
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

(defun condition-name (condition)
  (symbol-name (class-name (class-of condition))))

(defun condition-slots (condition)
  (mapcar #'closer-mop:slot-definition-name
	  (closer-mop:class-slots (class-of condition))))

(defun slot-values (obj)
  (loop for slot in (condition-slots obj)
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

(defun render (bt condition env)
  (let* ((backtrace (parse-backtrace bt)))
    (dev-page:index
     (list :name (condition-name condition)
           :slots (slot-values condition)
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

(defun render-prod (condition env)
  (prod-page:index (list :name (condition-name condition)
                         :url (getf env :path-info)
                         :css (slurp-file *prod-css-path*))))

(defparameter *clack-error-middleware*
  (lambda (app &key (debug t) (prod-render #'render-prod))
    (lambda (env)
      (handler-case
          (funcall app env)
        (t (condition)
          (let ((backtrace (with-output-to-string (stream)
                             (write-string (print-backtrace condition :output nil)
                                           stream))))
            (list 500
                  '(:content-type "text/html")
                  (list
                   (if debug
                       (render backtrace condition env)
                       (funcall prod-render condition env))))))))))
