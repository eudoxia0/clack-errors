(in-package :cl-user)
(defpackage clack-errors
  (:use :cl :clack :clack.response)
  (:export :<clack-error-middleware>))
(in-package :clack-errors)

(defclass <clack-error-middleware> (<middleware>) ())

(defun ex-name (ex)
  (symbol-name (mop-utils:class-name-of ex)))

(defun ex-slots (ex)
  (mop-utils:slot-names-of ex))

(defun slot-values (obj)
  (loop for slot in (ex-slots obj)
        collecting
        (list (symbol-name slot) (slot-value obj slot))))

(defparameter +backtrace-regex+ "\\n\\w*\\d+:"
  "A regular expression to split backtraces")

(defun split-backtrace (str)
  (ppcre:split +backtrace-regex+ str))

(defun parse-backtrace (bt ex)
  (destructuring-bind (header &rest frames) (split-backtrace bt)
    (let ((error-msg (subseq header
                             (position #\: header :from-end t)))
          (date-time (subseq header
                             (1+ (position #\: header))
                             (position #\A header))))
      (list error-msg date-time frames))))

(defun render (bt ex env)
  (let* ((backtrace (parse-backtrace bt ex)))
    (format t "~A" env)
    (error-page:index
     (list :name (ex-name ex)
           :slots (slot-values ex)
           :msg (nth 0 backtrace)
           :datetime (nth 1 backtrace)
           :backtrace (caddr backtrace)
           :url (getf env :path-info)
           :method (getf env :request-method)
           :query (getf env :query-string)))))
 
(defmethod call ((this <clack-error-middleware>) env)
  (handler-case (call-next this env)
    (t (ex)
      (let ((bt (trivial-backtrace:print-backtrace ex :output nil)))
        (list 200 '(:content-type "text/html")
              (list (render bt ex env)))))))
