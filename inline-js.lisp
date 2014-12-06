(defun parse-js (stream arg char)
  (declare (ignore arg char))
  (assert (eql #\s (read-char stream nil nil t)))
  (let* (args
         previous-1
         previous-2
         (string
           (with-output-to-string (result)
             (loop with quote
                   for char = (read-char stream nil nil t)
                   do
                   
                   (case char
                     (#\#
                      (cond ((and (eql previous-1 #\j)
                                  (eql previous-2 #\s))
                             (return))
                            ((push (read stream nil nil t) args)
                             (write-string "~a" result))))
                     (#\~
                      (write-string "~~" result))
                     (#\\
                      (write-char (if quote
                                      (read-char stream nil nil t)
                                      char)
                                  result))
                     ((#\' #\")
                      (write-char char result)
                      (cond ((eql quote char)
                             (setf quote nil))
                            ((not quote)
                             (setf quote char))))
                     (t
                      (shiftf previous-1 previous-2 char)
                      (write-char char result)))))))
    `(format nil ,(string-trim #(#\Space #\Newline)
                               (subseq string 0 (- (length string) 2)))
             ,@(nreverse args))))

(set-dispatch-macro-character #\# #\j
                              #'parse-js)
