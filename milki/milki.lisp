;;; MILKI - MInimal Lisp wiKI

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :asdf)
  (require :hunchentoot)
  (require :cl-who)
  (require :cl-store)
  (require :s-utils)
  (require :cl-fad))

(defpackage :milki
  (:use :common-lisp :cl-who)
  (:export #:start-wiki #:stop-wiki
	   #:add-user #:change-password #:remove-user))

(in-package :milki)

;; first we define some local paths
(defparameter *static-files-dir* "/home/lucindo/personal/lisp/milki/static/"
  "place where we can put all JavaScript and CSS files")
(defparameter *data-storage-dir* "/home/lucindo/personal/lisp/milki/data/"
  "place where we'll store all data")
(defparameter *upload-dir* "/home/lucindo/personal/lisp/milki/files/"
  "place to put uploaded files")

;; this will run on a multithreaded environment and for
;; some operations we will use a global lock
(defvar *lock* (hunchentoot-mp:make-lock "milki-lock"))

(defmacro locked (&body body)
  `(hunchentoot-mp:with-lock (*lock*) ,@body))

;; we need users
;; users are stored in a assoc-list: ("username" password)
;; the password is a md5 of password string
;; here we use the md5 package (required by hunchentoot)
;; the users list is stored in a file using cl-store
(defun users-file ()
  (concatenate 'string *data-storage-dir* ".users"))

; will be nil at first run
(defvar *user-alist* (ignore-errors (cl-store:restore (users-file))))

(defun sync-users-file ()
  (locked
    (cl-store:store *user-alist* (users-file))))

(defun add-user (user pass)
  (push (cons user (md5:md5sum-sequence pass)) *user-alist*)
  (sync-users-file))

(defun remove-user (user)
  (setf *user-alist* (remove-if #'(lambda (up-pair) (string= user (car up-pair))) *user-alist*))
  (sync-users-file))

(defun user-pass-match-p (user pass)
  (and user pass
       (equalp (cdr (assoc user *user-alist* :test #'string=))
	       (md5:md5sum-sequence pass))))

(defun change-password (user oldpass newpass)
  (when (and (user-pass-match-p user oldpass) newpass)
    (setf (cdr (assoc user *user-alist* :test #'string=))
	  (md5:md5sum-sequence newpass))
    (sync-users-file)))

;; now we define some utility macros to hunchentoot

; require a digest autorization
(defmacro with-authorization (&body body)
  (let ((user (gensym))
	(pass (gensym)))
    `(multiple-value-bind (,user ,pass)
	 (hunchentoot:authorization)
       (if (user-pass-match-p ,user ,pass)
	   ,@body
	   (hunchentoot:require-authorization "[milki login]")))))

; all wiki pages requires authorization (this is a private wiki!)
(defmacro with-wiki-page-body (&body body)
  `(with-authorization
     (with-html-output-to-string (*standard-output* nil :prologue t)
       (:html
	(:head
	 (:title "milki - a very simple wiki")
	 (:script :type "text/javascript" :src "/milki-static/showdown.js")
	 (:script :type "text/javascript" :src "/milki-static/milki.js")
	 (:link :rel "stylesheet" :href "/milki-static/milki.css" :type "text/css"))
	(:body ,@body)))))

;; we'll store wiki posts in files
(defun wiki-post-file-name (post-name)
  (concatenate 'string *data-storage-dir* "wiki-" (hunchentoot:url-encode post-name)))

(defun sync-wiki-post (post-name wiki-post-list)
  (locked
    (cl-store:store wiki-post-list (wiki-post-file-name post-name))))

; may return nil
(defun get-wiki-post (post-name)
  (locked
    (ignore-errors (cl-store:restore (wiki-post-file-name post-name)))))

;; a wiki post is very simple
(defclass wiki-post ()
  ((contents :initarg :contents
	     :accessor post-contents)
   (timestamp :initform (get-universal-time)
	      :accessor post-timestamp)
   (user :initarg :user
	 :accessor post-user)))

(defun add-wiki-post (post-name contents user)
  (let ((new-post (make-instance 'wiki-post :contents contents :user user))
	(post-list (get-wiki-post post-name)))
    (if post-list
	(push new-post post-list)
	(setf post-list (list new-post)))
    (sync-wiki-post post-name post-list)))

(defun show-wiki-post (post-name post-version post &optional (post-css-class "wiki-post"))
  (let ((edit-link (conc post-name "?edit=true&version=" (princ-to-string post-version))))
    (with-wiki-page-body 
      (:div :align "right"
	    (:font :size "-1"
		   (:a :href edit-link "edit")))
      (:div :id post-css-class :class post-css-class :ondblclick (conc "document.location=\"" edit-link "\";")
	    (str (post-contents post)))
      (:hr :class "footer")
      (:span :id "footer" :class "footer"
	     (:font :size "-1"
		    (:table :border "0" :width "98%" :align "center" :cellpadding "0" :cellspacing "0"
			    (:tr
			     (:td :align "left"
				  "useful links: "
				  (:a :href "/" "start page")
				  " - "
				  (:a :href "/milki-index" "wiki index"))
			     (:td :align "right"
				  (str (conc "last updated by " 
					     (post-user post) 
					     " on " 
					     (s-utils:format-universal-time (post-timestamp post))))))))))))

(defun print-wiki-post (post-name post-version)
  (let* ((wiki-post-list (get-wiki-post post-name))
	 (the-post (nth post-version wiki-post-list)))
    (cond ((not wiki-post-list)
	   (with-wiki-page-body (:center (:h2 "Hey! I'm an empty page!")
					 (:a :href (conc post-name "?edit=true")
					     (:h3 "edit me")))))
	  ((not the-post) (show-wiki-post post-name post-version (car wiki-post-list)))
	  (t (show-wiki-post post-name post-version the-post)))))

(defun edit-wiki-post (post-name post-version)
  (let* ((wiki-post-list (get-wiki-post post-name))
	 (the-post (nth post-version wiki-post-list)))
    (with-wiki-page-body
      (:center
       (:h2 (str (conc "editing: " post-name)))
       (:div :align "right"
	     (:font :size "-1"
		    (:a :href "/milki-upload" "click here to upload files")
		    (:br)
		    "use markdown syntax: cheatsheet "
		    (:a :href "javascript: open_markdown_cheatsheet()" "here")))
       (:form :method :post :action (str post-name)
	      (:table :with "98%" :border "0" :cellpadding "10" :cellspacing "2"
		      (:tr
		       (:td :width "48%" :valign "top"
			    (:textarea :id "wiki-input" :name "contents" :cols 60 :rows 20 :style "width: 100%;"
				       (when the-post (str (post-contents the-post)))))
		       (:td :width "48%" :valign "top" :style "border-left: solid 1px #736F6E"
			    (:div :id "wiki-preview")))
		      (:tr :rowspan "2"
			   (:td
				(:input :type :submit :value "save")
				(str " or ")
				(:a :href (princ-to-string post-name) "cancel"))))))
       (when  wiki-post-list
	 (htm
	  (:p "version history:")
	  (:ul
	   (loop for i from 0
	      for wiki-post in wiki-post-list
	      do (htm (:li (fmt "version ~a by ~a: " i (post-user wiki-post)) 
			   (:a :href (conc post-name "?version=" (princ-to-string i))
			       (str (conc (s-utils:format-duration (- (get-universal-time) 
								      (post-timestamp wiki-post))) 
					  " ago"))))))))))))

;; the main function handler
(defun milki ()
  (hunchentoot:no-cache)
  (let ((post-name (hunchentoot:script-name))
	(post-version (hunchentoot:get-parameter "version"))
	(edit-p (hunchentoot:get-parameter "edit"))
	(contents (hunchentoot:post-parameter "contents")))
    (if post-version
	(setq post-version (parse-integer post-version))
	(setq post-version 0))
    (when contents
      (add-wiki-post post-name contents (hunchentoot:authorization)))
    (if edit-p
	(edit-wiki-post post-name post-version)
      (print-wiki-post post-name post-version))))

; file-upload handler
(defun milki-upload ()
  (let ((sent-file (hunchentoot:post-parameter "file"))
	(remove-file (hunchentoot:get-parameter "remove")))
    (when remove-file
      (ignore-errors (delete-file (cl-fad:pathname-as-file (concatenate 'string *upload-dir* remove-file))))
      (hunchentoot:redirect (hunchentoot:script-name)))
    (when (and sent-file (listp sent-file))
      (let ((path (car sent-file))
	    (file-name (cadr sent-file)))
	;; strip directory info sent by Windows browsers
	(when (search "Windows" (hunchentoot:user-agent) :test #'char-equal)
	  (setq file-name (cl-ppcre:regex-replace ".*\\\\" file-name "")))
	(let ((new-path (concatenate 'string *upload-dir* file-name)))
	  (rename-file path (ensure-directories-exist new-path))))))
  (hunchentoot:no-cache)
  (let ((file-list (cl-fad:list-directory *upload-dir*)))
    (with-wiki-page-body
      (:center
       (:h2 "file uploader")
       (:form :method :post :enctype "multipart/form-data"
	      (:input :type :file :name "file")
	      (:input :type :submit :value "upload"))
       (when file-list
	 (htm
	  (:table :border "1" :width "95%"
		  (dolist (file file-list)
		    (let ((file-link-name (hunchentoot:url-encode (cl-ppcre:regex-replace ".*/" (princ-to-string file) ""))))
		      (htm (:tr
			    (:td (:a :href (conc "/milki-static/files/" file-link-name) (str file-link-name)))
			    (:td (str (conc "/milki-static/files/" file-link-name)))
			    (:td (:a :href (conc (hunchentoot:script-name) "?remove=" file-link-name) "remove")))))))))))))

;; Wiki page index

(defun generate-wiki-index ()
  (let ((wiki-pages-list ())
	(wiki-files (cl-fad:list-directory *data-storage-dir*)))
    (dolist (file-path wiki-files)
      (let ((file-name (cl-ppcre:regex-replace ".*/" (princ-to-string file-path) "")))
	(when (string= (subseq file-name 0 5) "wiki-")
	  (push (hunchentoot:url-decode (subseq file-name 5)) wiki-pages-list))))
    (sort wiki-pages-list #'string<)))

; index handler
(defun milki-index ()
  (let ((wiki-pages (generate-wiki-index)))
    (with-wiki-page-body
      (:center (:h2 "wiki index")
	       (:h4 (fmt "~a pages so far..." (length wiki-pages))))
      (:ul
       (dolist (page wiki-pages)
	 (htm (:li (:a :href page (str page)))))))))

;; finally we setup hunchentoot environment

(eval-when (:execute :load-toplevel)
  (setf hunchentoot:*show-lisp-errors-p* t
	hunchentoot:*show-lisp-backtraces-p* t
	hunchentoot:*dispatch-table* 
	(list 'hunchentoot:dispatch-easy-handlers
	      (hunchentoot:create-folder-dispatcher-and-handler "/milki-static/files/" *upload-dir*)
	      (hunchentoot:create-folder-dispatcher-and-handler "/milki-static/" *static-files-dir*)
	      (hunchentoot:create-prefix-dispatcher "/milki-upload" 'milki-upload)
	      (hunchentoot:create-prefix-dispatcher "/milki-index" 'milki-index)
	      (hunchentoot:create-prefix-dispatcher "/" 'milki))))

; we need a variable to hold the server instance
(defvar *wiki-server* nil "the hunchentoot server instance")

(defun start-wiki (&key (port 8080))
  (setf *wiki-server* (hunchentoot:start-server :port port)))

(defun stop-wiki ()
  (hunchentoot:stop-server *wiki-server*))
