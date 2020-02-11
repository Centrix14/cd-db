(defvar *db* nil)
(defvar *query-io* nil)

(defun make-cd (title artist rating ripped)
  (list :title title :artist artist :rating rating :ripped ripped))

(defun add-record (cd) 
  (push cd *db*))

(defun dump-db ()
  (format t "~%~10t~a~%~%" *db*))

(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun prompt-for-cd ()
  (make-cd 
	(prompt-read "Title")
	(prompt-read "Artist")
	(or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
	(y-or-n-p "Ripped [y/n]")))

(defun add-cds ()
  (loop (add-record(prompt-for-cd))
		(if (not (y-or-n-p "Another? [y/n]: ")) (return))))

(defun save-db (filename)
  (with-open-file (out filename
					   :direction :output
					   :if-exists :supersede)
	(with-standard-io-syntax
	  (print *db* out))))

(defun load-db (filename)
  (with-open-file (in filename)
	(with-standard-io-syntax
	  (setf *db* (read in)))))

(defun where (&key title artist rating (ripped nil ripped-p))
  #'(lambda (cd)
	  (and
		(if title (equal (getf cd :title) title) t)
		(if artist (equal (getf cd :artist) artist) t)
		(if rating (equal (getf cd :rating) rating) t)
		(if ripped-p (equal (getf cd :ripped) ripped) t))))

(defun select (selector-fn)
  (remove-if-not selector-fn *db*))

(if (y-or-n-p "Load?") (load-db (prompt-read "Enter file name")))
(if (y-or-n-p "Add new CD?") (add-cds))
(if (y-or-n-p "Show DB?") (dump-db))
(if (y-or-n-p "Save DB?") (save-db (prompt-read "Enter file name")))

(print (select (where :artist "LP")))
