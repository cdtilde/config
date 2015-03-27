;;; data-loader.el --- Data buffer summary!
;; Convert a delimited file (tab, comma or pipe) to
;; a temporary table on Teradata


;; ----------------------------------------------------------------------------------------------------
;;
;; Variables and Color Definitions
;;
;; ----------------------------------------------------------------------------------------------------

;;; Commentary:
;; 

;;; Code:

;(defvar data-mode-current-highlight-number "1" "The number of the current highlight face.")


;; ----------------------------------------------------------------------------------------------------
;;
;; Interactive functions - should be bound to shortcut keys
;;
;; ----------------------------------------------------------------------------------------------------

(defun top-of-column ()
  (interactive)
  (let ((col (current-column)))
    (goto-beginning-of-data)
    (forward-char col)))
    

;; ----------------------------------------------------------------------------------------------------
;;
;; Table creation and formatting functions
;;
;; ----------------------------------------------------------------------------------------------------



(defun load-table (file-name table-name)
  "Load a tab-delimited table from disk and convert to SQL statements."
  (interactive "f\nsTable Name: ")
   (get-buffer-create "foo")
  (with-temp-buffer
   (insert-file-contents file-name)
    (find-file file-name)
    (flush-lines "^[ ]*$") ; remove blank lines
    (goto-char 1)
    (let ((column-names (all-regexp-matches  "[^\t\n]*" (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))
      (kill-buffer "*sql-temp*")
      (switch-to-buffer-other-window "*sql-temp*")      
      (insert-string (format "CREATE MULTISET TABLE shc_work_tbls.%s (\n" table-name))
      (mapcar (lambda (arg)
                (insert-string (format "%s varchar(255)%s\n" arg (if (eq arg (car (last column-names))) "" ","))))
              column-names)
      (insert-string ")")

      
      (goto-char 0)
      (forward-line 1)
      (beginning-of-line)
      (while (and (< num-errors stop-after)
                  (< (point) (point-max))
                  (< (point) end))
        (setq num-lines-visited (1+ num-lines-visited))
        (setq result (apply myfunc parms))
        (cond ((= 1  result) (setq num-acted-on-by-myfunc (1+ num-acted-on-by-myfunc)))
              ((= -1 result) (setq num-errors (1+ num-errors)))
              ((= -2 result) (setq num-skipped (1+ num-skipped))))
        (when (not (= -3 result))
          (forward-line 1)))
      


      )


    ))






(defun format-table ()
  (interactive)
  "Format the table per `data-mode' conventions:
- Columns are encolosed in | |
- The second line is a separator (+-----+-----+ etc.)
- The first column is a special marker column (|    |)
- There is no whitespace after the end of the table"
  (save-excursion
    (beginning-of-buffer)  
    (beginning-of-buffer)
    (when (not (string= "|   |" (buffer-substring-no-properties 1 6)))    
      (add-pipes-before-after)
      (add-header-delimiter)
      (add-column-numbers))))

(defun add-pipes-before-after ()
  (interactive)
  (beginning-of-buffer)
  (beginning-of-line)
  (insert "|   |")
  (end-of-line)
  (insert " |")
  (let ((last-char-line (current-column)))
    (forward-line)
    (while (< (point) (point-max))
      (beginning-of-line)
      (insert "|   |")
      (end-of-line)
      (while (< (current-column) (- last-char-line 2))
        (insert " "))
      (insert " |")
      (forward-line 1))))

(defun add-header-delimiter ()
  "Add a row below the first row of the table with a horizontal line that looks like this:
+-----+-----+ etc.

The first row of the table is defined as the first non-blank line above point, or if no blank line above point, the first line of the buffer."
  (interactive)
  (save-excursion
    (setq inhibit-field-text-motion t)
    (beginning-of-buffer)
    (forward-line)
    (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
      (while (string-match "|\\([^|]*\\)|" line)
        (setq line (replace-match
                    (concat "+" (make-string (- (match-end 1) (match-beginning 1))
                                             ?-) "|") t t line)))
      (when (string-match "\\(|\\) *$" line)
        (setq line (replace-match "+" nil nil line 1)))      
      (insert line)
      (newline))))

(defun add-column-numbers ()
  (interactive)
    (save-excursion
    (setq inhibit-field-text-motion t)
    (beginning-of-buffer)
    (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
          (colnum 1)
          (colname "")
          (start-pos 0))
      (while (and (string-match "|\\([^|]*\\)|" line start-pos) (< colnum 100))
        (setq colname (concat "$" (number-to-string colnum)))
        (setq replacement-string
              (concat (make-string (/ (- (match-end 1) (match-beginning 1) (length colname)) 2)  ? )
                      colname
                      (make-string (/ (- (match-end 1) (match-beginning 1) (length colname)) 2) ? )))
        (when (< (length replacement-string) (- (match-end 1) (match-beginning 1)))
                 (setq replacement-string (concat " " replacement-string)))        
        (setq line (replace-match replacement-string nil nil line 1))
        (setq colnum (1+ colnum))
        (setq start-pos (match-end 1)))
      (beginning-of-buffer)
      (insert line)
      (newline))))




(defun apply-defun-to-region-lines (beg end stop-after myfunc &rest parms)
  "Apply a function to all lines in the region marked by BEG and END, passing PARMS as the arguments to MYFUNC.

Stop processing after encountering STOP-AFTER number of errors. If nil, do not stop.

Returns a list with:
- how many lines were iterated over
- how many were acted on by myfunc
- how many produced an error
- how many lines were skipped
- t if evaluation was stopped because we reached STOP-AFTER errors

myfunc is expected to return:
- 1 if an action was taken on the line
- 0 if no action was taken on the line
- -1 if an error occurred
- -3 if apply-defun-to-region-lines should NOT move to the next line"
  (save-excursion
    (let ((num-lines-visited 0)
          (num-acted-on-by-myfunc 0)
          (num-errors 0)
          (num-skipped 0)
          (result nil))
      (goto-char beg)
      (beginning-of-line)
      (while (and (< num-errors stop-after)
                  (< (point) (point-max))
                  (< (point) end))
        (setq num-lines-visited (1+ num-lines-visited))
        (setq result (apply myfunc parms))
        (cond ((= 1  result) (setq num-acted-on-by-myfunc (1+ num-acted-on-by-myfunc)))
              ((= -1 result) (setq num-errors (1+ num-errors)))
              ((= -2 result) (setq num-skipped (1+ num-skipped))))
        (when (not (= -3 result))
          (forward-line 1)))
      (list num-acted-on-by-myfunc  num-lines-visited  num-skipped num-errors (>= num-errors stop-after)))))


(defun log-error (message file pos)
  (save-excursion
    (switch-to-buffer-other-window "*data-mode results*")
    (compilation-mode)
    (read-only-mode 0)
    (insert (format "\n%s:%s:1: %s" file pos message))
    (read-only-mode 1)))





(provide 'data-loader)


;;; data-mode.el ends here
