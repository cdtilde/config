
(defun count-items (calculation)
  (interactive)
  (message "XXX %s" (org-table-get 3 1)))

(defun mark-calc (&optional beg end nlast)
  (interactive)
  (save-excursion
    (let (col (org-timecnt 0) diff h m s org-table-clip)
      (cond
       ((and beg end))   ; beg and end given explicitly
       ((org-region-active-p)
	(setq beg (region-beginning) end (region-end)))
       (t
	(setq col (org-table-current-column))
	(goto-char (org-table-begin))
	(unless (re-search-forward "^[ \t]*|[^-]" nil t)
	  (user-error "No table data"))
	(org-table-goto-column col)
	(setq beg (point))
	(goto-char (org-table-end))
	(unless (re-search-backward "^[ \t]*|[^-]" nil t)
	  (user-error "No table data"))
	(org-table-goto-column col)
	(setq end (point))))
      (let* ((items (apply 'append (org-table-copy-region beg end)))
	     (items1 (cond ((not nlast) items)
			   ((>= nlast (length items)) items)
			   (t (setq items (reverse items))
			      (setcdr (nthcdr (1- nlast) items) nil)
			      (nreverse items))))
	     (numbers (delq nil (mapcar 'org-table-get-number-for-summing
					items1)))
             
	     (res (apply '+ numbers))
	     (sres (if (= org-timecnt 0)
		       (number-to-string res)
		     (setq diff (* 3600 res)
			   h (floor (/ diff 3600)) diff (mod diff 3600)
			   m (floor (/ diff 60)) diff (mod diff 60)
			   s diff)
		     (format "%.0f:%02.0f:%02.0f" h m s))))
	(kill-new sres)
        (message "Numbers: %s" numbers)
	(if (org-called-interactively-p 'interactive)
	    (message "%s"
		     (substitute-command-keys
		      (format "Sum of %d items: %-20s     (\\[yank] will insert result into buffer)"
			      (length numbers) sres))))
	sres))))




(defun filter (beg end)
  (interactive "r")
  (save-excursion
    (let ((first-row 0)
          (last-row 0)
          (cur-row 0)
          (cur-col org-table-current-column))      
      (cond
       ((and beg end))
       ((org-region-active-p)
        (goto-char (region-beginning))
        (setq first-row (org-table-current-line))
        (goto-char (region-end))
        (setq beg (region-beginning) end (region-end)))
       (t
        (goto-char (org-table-end))
        (setq first-row 2)
          (last-row (org-table-current-line))
          (cur-row 2))
      (message "First %s last %s current %s" first-row last-row cur-row)
      (while (< cur-row last-row)
        (message "Value in column 6: %s" (org-table-get cur-row 6))
        (setq cur-row (1+ cur-row))))))

  


;; todo
;; - only show lines marked in yellow, purple, red
;; - only show lines with region marking in yellow, purple, red
;; - show all lines with any markings
;; - same: delete
;; - same: invert
;; - column totals in formulas
;; - previous row in formulas
;; - variable ranges in formuals (columns 1- 4)
;; - overlay with column numbers
;; - info-at-point (which column)
;; X handle column headers
;; - fix region or this line
;; - integration column calc functions
;; - histogram function
;; - regexp in addition to calculation
;; - only act on already marked lines with second pass of marking
;; - prompts when asking?
;; - Import a table from SQL

;; Variables
(defvar data-mode-current-highlight-face 'data-mode-highlight-1 "The face that will be used for the next highlighting operation")
(defvar stop-marking-after-errors 10 "After this many calculation errors, stop doing mark-line-or-region") 



(defface data-mode-highlight-1
       '((((class color) (min-colors 88) (background light))
          :background "gold1" :foreground "black")
         (((class color) (min-colors 88) (background dark))
          :background "gold1" :foreground "black")
         (((class color) (min-colors 16) (background light))
          :background "gold1" :foreground "black")
         (((class color) (min-colors 16) (background dark))
          :background "gold1" :foreground "black")
         (((class color) (min-colors 8))
          :background "gold1" :foreground "black")
         (t :inverse-video t))
       "Basic face for highlighting."
       :group 'data-mode-faces)


(defface data-mode-highlight-2
       '((((class color) (min-colors 88) (background light))
          :background "purple1" :foreground "white")
         (((class color) (min-colors 88) (background dark))
          :background "purple1" :foreground "white")
         (((class color) (min-colors 16) (background light))
          :background "purple1" :foreground "white")
         (((class color) (min-colors 16) (background dark))
          :background "purple1" :foreground "white")
         (((class color) (min-colors 8))
          :background "purple1" :foreground "white")
         (t :inverse-video t))
       "Basic face for highlighting."
       :group 'data-mode-faces)

(defface data-mode-highlight-3
       '((((class color) (min-colors 88) (background light))
          :background "red" :foreground "white")
         (((class color) (min-colors 88) (background dark))
          :background "red" :foreground "white")
         (((class color) (min-colors 16) (background light))
          :background "red" :foreground "white")
         (((class color) (min-colors 16) (background dark))
          :background "red" :foreground "white")
         (((class color) (min-colors 8))
          :background "red" :foreground "white")
         (t :inverse-video t))
       "Basic face for highlighting."
       :group 'data-mode-faces)


;; 
;; MARK MANUALLY
;; 

;; Functions that operate on the exact region
(defun mark-region (beg end)
  "Apply the currently selected highlighting face to the region. Use to mark characters or strings (parts of lines)."
  (interactive "r")
  (overlay-put (make-overlay beg end) 'face  data-mode-current-highlight-face))

(defun unmark-region (beg end)
  "Remove all data-mode faces from the region."
  (interactive "r")
  (mapcar (lambda (arg)
            (if (eq (type-of (overlay-get arg 'face)) 'symbol)
                (setq face-name (symbol-name (overlay-get arg 'face)))
              (setq face-name (overlay-get arg 'face)))
            (if (string-match "data-mode" face-name)
                (delete-overlay arg)))
          (overlays-in beg end )))


;; Functions that operate on whole lines
(defun mark-current-line-or-lines-in-region (beg end)
  "Apply the currently selected highlighting face to the lines in the region, or if there's no active region, the current line. When applying it to the region, widen the selection to include complete lines."
  (interactive "r")
  (save-excursion
    (widen-region-to-whole-lines beg end)    
    (mark-region (point) (mark))))

(defun unmark-current-line-or-lines-in-region (beg end)
  "Remove all data-mode faces from the lines in the region, or if there's no active region, the current line. When applying it to the region, widen the selection to include complete lines."  
  (interactive "r")
  (save-excursion
    (widen-region-to-whole-lines beg end)    
    (unmark-region (mark) (point))))


;; 
;; MARK BY CALCULATING WITH NUMBERS
;; 


(defun mark-region-by-calculation (beg end calculation)
  "Evaluate the CALCULATION on all lines in the region BEG END, and highlight all lines where CALCULATION is true. If no region, operate on the entire buffer.

CALCULATION is any valid GNU Calc calculation, using column variables to refer to the data on the current line. Column variables are of the form $X, where X is the number of the column, with the leftmost column being 1. If a variable is not given in CALCULATION, determine the column where point is currently, and prepend it to CALCULATION. 

Examples:
1) Mark all lines where column 7 is at least 20% greater than column 8

   $7 >= (1.2*$8)

2) Mark all lines where the current column is less than 10

   < 10

   In this example, since no column variable is referenced, the calculation will be modified before evaluating. Assuming point is in column 12:

   $12 < 10
"

  (interactive "r\nsFilter: ")
  (message "Evaluating... (C-g to quit)")
  (ignore-errors (kill-buffer "*data-mode results*"))
  (setq matched-lines 0)
  (setq calc-eval-error nil)
  (save-excursion
    (if (not (string-match "$[0-9]*" calculation))
        (setq calculation
              (concat "$"
                      (number-to-string (count-matches "|"  (line-beginning-position) (point) nil))
                      " "
                      calculation)))
    (if (not (use-region-p))
        (progn
          (beginning-of-buffer)
          (if table-has-header              
              (forward-line 1))
          (setq beg (point))
          (end-of-buffer)
          (setq end (point))
          (message "Did progn")))
    (let ((variables '()))
      (let ((start-pos 0))
        (while (string-match "$\\([0-9]*\\)" calculation start-pos)
          (push (string-to-number (match-string 1 calculation)) variables)
          (setq start-pos (match-end 0))))    
      (setq results (apply-defun-to-region-lines beg end stop-marking-after-errors 'mark-line-by-calculation calculation variables))
      (message "Marked: %s lines, Visited: %s, Skipped: %s, Errors: %s %s %s"
                (nth 0 results)
                (nth 1 results)
                (nth 2 results)
                (nth 3 results)
                (if (nth 4 results) "    --- STOPPED evaluating after reaching error limit" "")
                (if table-has-header " (table has header)" " (table has no header)")))))
  


(progn
  (beginning-of-buffer)
  (forward-line 1)
  (set-mark-command nil)
  (end-of-buffer))



;; TODO: clear out variables afterwards...
(defun mark-line-by-calculation (calculation variables)
 (setq values (split-string
                (buffer-substring-no-properties
                 (line-beginning-position)
                 (line-end-position)) "|" t))
 (setq values (cons "" values))
 (setq line-calculation calculation) 
 (catch 'line-should-be-ignored
   (mapc (lambda (arg)
           (if (or (not (nth arg values)) (string-match ignore-columns-regexp (nth arg values))) (throw 'line-should-be-ignored -2))
           (setq line-calculation 
                 (replace-regexp-in-string 
                  (concat "$" (number-to-string arg)) (nth arg values)
                  line-calculation)))
         variables)
   (setq result (ignore-errors (calc-eval line-calculation)))
   (cond ((or (listp result) (consp result) (not result))
          (log-error (format "Error in formula %s" line-calculation) buffer-file-name (line-number-at-pos)) -1)          
         ((string= "1" result)
          (progn
            (mark-current-line-or-lines-in-region  (line-beginning-position) (line-end-position))
            1))
         (t 0))))



(defun set-highlight-color (arg)
  "Set the current highlighting color for data-mode. The color number selected will be used for highlighting and marking."
  (setq data-mode-current-highlight-face (concat "data-mode-highlight-" arg)))
;; Helper functions

(defun widen-region-to-whole-lines (beg end)
  "With an active region, move the point to the beginning of the first line and the mark to the end of the last line. 

With no active region, do the same to the current line."
  (if (not (use-region-p))
        (progn
          (beginning-of-line)
          (set-mark (point))
          (end-of-line))
      (progn
        (goto-char beg)
        (beginning-of-line)
        (set-mark (point))
        (goto-char end)
        (end-of-line))))
    


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
"

  (save-excursion
    (let ((end-marker (copy-marker end))
          (num-lines-visited 0)
          (num-acted-on-by-myfunc 0)
          (num-errors 0)
          (num-skipped 0))
      (goto-char beg)
      (beginning-of-line)
      (setq next-line-marker (point-marker))
      (while (and (< num-errors stop-after) (< next-line-marker end-marker))
        (setq num-lines-visited (1+ num-lines-visited))
        (goto-char next-line-marker)
        (setq result (apply myfunc parms))
        (cond ((= 1  result) (setq num-acted-on-by-myfunc (1+ num-acted-on-by-myfunc)))
              ((= -1 result) (setq num-errors (1+ num-errors)))
              ((= -2 result) (setq num-skipped (1+ num-skipped))))
        (save-excursion
          (forward-line 1)
          (set-marker next-line-marker (point))))
      (set-marker end-marker nil)
      (set-marker next-line-marker nil)
      (list num-acted-on-by-myfunc  num-lines-visited  num-skipped num-errors (>= num-errors stop-after)))))



(defun log-error (message file pos)
  (save-excursion
    (switch-to-buffer-other-window "*data-mode results*")
    (compilation-mode)
    (read-only-mode 0)
    (insert (format "\n%s:%s:1: %s" file pos message))
    (read-only-mode 1)))



  

;; Mode definition
(define-derived-mode data-mode nil "Data Mode"
  "Major mode for editing rows/columns of data"
  (toggle-truncate-lines 1))

(define-key data-mode-map (kbd "m") 'mark-current-line-or-lines-in-region)
(define-key data-mode-map (kbd "u") 'unmark-current-line-or-lines-in-region)
(define-key data-mode-map (kbd "h") 'mark-region)
(define-key data-mode-map (kbd "e") 'unmark-region)


(define-key data-mode-map (kbd "1") (lambda () (interactive) (set-highlight-color "1")))
(define-key data-mode-map (kbd "2") (lambda () (interactive) (set-highlight-color "2")))
(define-key data-mode-map (kbd "3") (lambda () (interactive) (set-highlight-color "3")))



(provide 'data-mode)


(defun exfilter (calculation)
  (interactive "sFilter expression: ")
  (let* ((thisline (org-current-line))
	 (thiscol (org-table-current-column))
	 (otc org-table-overlay-coordinates)
	 beg end bcol ecol tend tbeg column lns pos)
    (when (equal thiscol 0)
      (if (org-called-interactively-p 'any)
	  (setq thiscol
		(string-to-number
		 (read-string "Use column N for sorting: ")))
	(setq thiscol 1))
      (org-table-goto-column thiscol))
    (org-table-check-inside-data-field)
    (if (org-region-active-p)
	(progn
	  (setq beg (region-beginning) end (region-end))
	  (goto-char beg)
	  (setq column (org-table-current-column)
		beg (point-at-bol))
	  (goto-char end)
	  (setq end (point-at-bol 2)))
      (setq column (org-table-current-column)
	    pos (point)
	    tbeg (org-table-begin)
	    tend (org-table-end))
      (if (re-search-backward org-table-hline-regexp tbeg t)
	  (setq beg (point-at-bol 2))
	(goto-char tbeg)
	(setq beg (point-at-bol 1)))
      (goto-char pos)
      (if (re-search-forward org-table-hline-regexp tend t)
	  (setq end (point-at-bol 1))
	(goto-char tend)
	(setq end (point-at-bol))))
    (setq beg (move-marker (make-marker) beg)
	  end (move-marker (make-marker) end))
    (untabify beg end)
    (goto-char beg)
    (org-table-goto-column column)
    (skip-chars-backward "^|")
    (setq bcol (current-column))
    (org-table-goto-column (1+ column))
    (skip-chars-backward "^|")
    (setq ecol (1- (current-column)))
    (org-table-goto-column column)
    (goto-char beg)
    (let ((variables '())
          (lines-marked 0)
          (lines-visited 0)
          (lines-errored 0)
          (stopped-because-of-errors nil))
      (if (not (string-match "$[0-9]*" calculation))
          (setq calculation
                (concat "$"
                        (number-to-string column)
                        " "
                        calculation)))
      (let ((start-pos 0))        
        (while (string-match "$\\([0-9]*\\)" calculation start-pos)
          (push (string-to-number (match-string 1 calculation)) variables)
          (setq start-pos (match-end 0))))
      (while (< (point) end)
        (setq lines-visited (1+ lines-visited))
        (setq line-calculation calculation)
        (mapc (lambda (arg)
                (setq line-calculation 
                      (replace-regexp-in-string 
                       (concat "$" (number-to-string arg)) (org-table-get (org-table-current-line) arg)
                       line-calculation)))
              variables)
        (setq result (ignore-errors (calc-eval line-calculation)))
        (cond ((or (listp result) (consp result) (not result))
               (log-error (format "Error in formula %s" line-calculation) buffer-file-name (line-number-at-pos))
               (setq lines-errored (1+ lines-errored)))          
              ((string= "1" result)
               (progn
                 (mark-current-line-or-lines-in-region  (line-beginning-position) (line-end-position))
                 (setq lines-marked (1+ lines-marked)))))
        (forward-line))
        (message  "Marked: %s lines, Visited: %s, Errors: %s "
                  lines-marked lines-visited lines-errored)
                  )))

        




    
    (mapc (lambda(x) (message "My value %s, my line is %s, my column is %s"
                              (org-sort-remove-invisible
                               (nth (1- column)
                                    (org-split-string x "[ \t]*|[ \t]*")))
                                start-line column))
          (org-split-string (buffer-substring beg end) "\n"))))


  

    (message "%d lines sorted, based on column %d" (length lns) column)))
