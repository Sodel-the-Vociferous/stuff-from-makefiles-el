(require 'async)
(require 'cl)

(defcustom sfmf-regex-alist
  '((:curdir . "^CURDIR :?= \\(?1:[[:graph:]]+\\)$")
    (:include-dir . "\\(^\\|.*?[[:blank:]]\\)-I[[:blank:]]*\\(?1:[[:graph:]]+\\)"))
  "((thing-type . regex) ...)")

(defun sfmf--make-output (&optional makefile)
  (apply 'process-lines
         "make" "-pn"
         (when makefile
           (list "-C" (or (file-name-directory makefile)
                          ".")
                 "-f" makefile))))

(defun sfmf--line-parse (line)
  (let (line-alist)
    (dolist (regexp-cons sfmf-regex-alist line-alist)
      (destructuring-bind (thing-type . regexp) regexp-cons
        (let ((start 0))
          (while start
            (let ((case-fold-search nil))
              (setq start (string-match regexp line start)))
            (when start
              (push (cons thing-type (match-string 1 line))
                    line-alist)
              (setq start (match-end 0)))))))))

(defun sfmf--list-include-dirs (line-alists)
  (remove-duplicates
   (let (include-dirs curdir)
     (dolist (line-alist line-alists include-dirs)
       (dolist (pair line-alist)
         (destructuring-bind (thing-type . val) pair
           (when val
             (cond ((eq thing-type :curdir)
                    (setq curdir val))
                   ((eq thing-type :include-dir)
                    (push (expand-file-name val curdir)
                          include-dirs))
                   (t nil)))))))
   :test 'equal))

(defun sfmf-read (&optional makefile)
  (let* ((lines (sfmf--make-output makefile))
         (line-alists (delete-if 'null (mapcar 'sfmf--line-parse lines))))
    (list :include-dirs (sfmf--list-include-dirs line-alists))))

(sfmf-read "~/src/navsim/RG5/proto1_compile_dangit/Makefile")
