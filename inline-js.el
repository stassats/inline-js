(defun inline-js-substring ()
  (save-excursion
   (let* ((position (point))
         (line (line-number-at-pos))
         (start (re-search-backward "[^\\]#js" nil t))
         (start-line (line-number-at-pos))
         (end (and start
                   (re-search-forward "js#" nil t))))
     (when (and end start
                (> end position))
       (values (buffer-substring-no-properties (+ start 4) (- end 3))
               line (1+ (- line start-line))
               (+ start 4)
               (- end 3))))))

(defun inline-js-indent-line ()
  (multiple-value-bind (string line offset-line start end)
      (inline-js-substring)
    (cond (string
           (let ((column (current-column)))
             (delete-region start end)
             (insert (with-temp-buffer
                         (insert string)
                       (goto-char (point-min))
                       (forward-line (1- offset-line))
                       (javascript-mode)
                       (move-to-column column)
                       (js-indent-line)
                       (setf column (current-column))
                       (buffer-string)))
             (goto-char (point-min))
             (forward-line (1- line))
             (move-to-column column)))
          (t
           (lisp-indent-line)))))

(add-hook 'lisp-mode-hook
          (lambda ()
            (set (make-local-variable 'indent-line-function)
                 'inline-js-indent-line)))
