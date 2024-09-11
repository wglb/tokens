;;;; tokens.lisp
;; from graham

(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0)))


(defpackage #:tokens
  (:use #:cl)
  (:export #:constituent
           #:tokens
		   #:tokenize1
		   #:tokensi))

(in-package #:tokens)

(defun constituent(c)
  (and (graphic-char-p c)
       (not (char= c #\ ))))

(defun tokens (str test start)
  "from graham pcl p 67"
  (let ((p1 (position-if test str :start start)))
    (if p1
        (let ((p2 (position-if #'(lambda (c)
                                   (not (funcall test c)))
                               str :start p1)))
          (cons (subseq str p1 p2)
                (if p2 
                    (tokens str test p2)
                    nil)))
        nil)))

(defun tokenize1 (str chr)
  (tokens str #'(lambda (ch)
				  (not (char= ch chr)))
		  0))

(defun tokensi (str test)
  "Tokenize using non-recursive"
  (let ((ans nil))
	(do* ((p1 (position-if test str) (if p2 (position-if test str :start p2) nil))
		  (p2 p1 (if p1
					 (position-if #'(lambda (c)
								 (not (funcall test c)))
							 str :start p1)
					 nil)))
		 ((and (null p1) (null p2)))
	  (unless (or (and (zerop p1) (if p2
									  (zerop p2)
									  t))
				  
				  (equal p1 p2))
		(push (subseq str p1 p2) ans)))
	(nreverse ans)))

