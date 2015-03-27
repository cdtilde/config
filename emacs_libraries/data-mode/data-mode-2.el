;;
;;
;;

;; Objects:
;; - Line 
;;   - All data lines
;;   - Only data lines in a region

;; - Column (calculation, filter)
;; - Text (highlight)

;; - Cell




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
(defvar data-mode-current-highlight-number "1" "The number of the current highlight face")
(defvar data-mode-current-highlight-face 'data-mode-highlight-1 "The face that will be used for the next highlighting operation")
(defvar stop-marking-after-errors 10 "After this many calculation errors, stop doing mark-line-or-region")
(defvar ignore-columns-regexp ".*\\?.*" "Ignore any column value that matches this regular expression. Applies to filtering by calculation, etc.")


;; Color definitions
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


(defun set-highlight-color (arg)
  "Set the current highlighting color for data-mode. The color number selected will be used for highlighting and marking."
  (setq data-mode-current-highlight-number arg)
  (setq data-mode-current-highlight-face (concat "data-mode-highlight-" arg)))



;; 
;; MARK MANUALLY
;; - region
;; - line(s)
;; 

;; Functions that operate on the exact region
(defun mark-region (beg end)
  "Apply the currently selected highlighting face to the region. Use to mark characters or strings (parts of lines)."
  (interactive "r")
  (overlay-put (make-overlay beg end) 'face  data-mode-current-highlight-face))

(defun unmark-all-lines ()
  (interactive)
  (save-excursion
    (let* ((beg (progn (goto-beginning-of-data) (point)))
           (end (progn (goto-end-of-data) (point))))
      (unmark-current-line-or-lines-in-region beg end))))
    

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
    (apply-defun-to-region-lines (mark) (point) 100 '(lambda ()
                                                (mark-region (line-beginning-position) (line-end-position))
                                                (save-excursion (beginning-of-line)
                                                                (forward-char 2)
                                                                (delete-char 1)
                                                                (message "hn %s %s" data-mode-current-highlight-number (type-of data-mode-current-highlight-number))
                                                                (insert data-mode-current-highlight-number))
                                                0))))
                                                

(defun unmark-current-line-or-lines-in-region (beg end)
  "Remove all data-mode faces from the lines in the region, or if there's no active region, the current line. When applying it to the region, widen the selection to include complete lines."
  (interactive "r")
  (message "Beg %s end %s" beg end)
  (save-excursion
    (widen-region-to-whole-lines beg end)
    (apply-defun-to-region-lines (mark) (point) 100 '(lambda ()
                                                       (message "Beg %s" (line-beginning-position))
                                                       (unmark-region (line-beginning-position) (line-end-position))
                                                       (save-excursion
                                                         (beginning-of-line)
                                                         (forward-char 2)
                                                         (delete-char 1)
                                                         (insert " "))
                                                       0))))
    
    
    




;; Sorting and summarizing


(defun summarize-column ()
       "Add all the lines in the region-rectangle and put the result in the 
        kill ring."
       (interactive)
       (setq pos (rectangle-of-current-column))
       (let ((sum 0) (max most-negative-fixnum) (min most-positive-fixnum) (count 0))         
         (mapc (lambda (line)
                 (let ((number (make-number line)))
                       (setq sum (+ sum number))
                       (if (> number max)
                           (setq max number))
                       (if (< number min)
                           (setq min number))
                       (setq count (+ count 1))))               
               (extract-rectangle (car pos) (cadr pos)))
         (kill-new (number-to-string sum))
         (message "Sum: %s, Min: %s, Max: %s, Avg: %s, Count: %s" sum min max (/ sum count) count)))

(defun sort-column (&optional reverse-sort)
  "With point inside a column (between two | delimiters), sort the column using sort-columns"
  (interactive "P")
  (setq mycolumn (current-column))
  (save-excursion 
    (setq pos (rectangle-of-current-column))
    (sort-columns reverse-sort (car pos) (cadr pos)))
    (move-to-column mycolumn))



(defun rectangle-of-current-column ()
  (interactive)
  ;(save-excursion
    (search-backward "|" nil)
    (forward-char)
    (setq column-start (current-column))
    (search-forward "|" nil)
    (backward-char)
    (setq column-end (current-column))
    (goto-end-of-data)
    (move-to-column column-end)
    (setq end (point))
    (goto-beginning-of-data)
    (move-to-column column-start)
    (setq beg (point))
    (setq pos (list beg end))
    (list beg end))


(defun is-current-line-marked ()
  (interactive)
  (message "I'm looking at %s" (buffer-substring-no-properties (line-beginning-position) (+ (line-beginning-position) 20)))
  (message "Overlays: %s" (overlays-in (line-beginning-position) (line-end-position)))
  (message "b %s e %s" (line-beginning-position) (line-end-position))
  (let ((ismarked nil))
    (mapc (lambda (arg)
            (setq face-name nil)
            (if (eq (type-of (overlay-get arg 'face)) 'symbol)
                (setq face-name (symbol-name (overlay-get arg 'face)))
              (setq face-name (overlay-get arg 'face)))
            (message "Face name %s" face-name)
            (when (string-match data-mode-current-highlight-face face-name)
              (message "Setting to t")
              (setq ismarked t)))                
          (overlays-in (line-beginning-position) (line-end-position)))
    (message "Returning %s" ismarked)
   ismarked))
    
    
;; Delete marked lines
(defun delete-marked-lines (beg end)
  "Remove all data-mode faces from the region."
  (interactive "r")
  (when (not (use-region-p))
    (goto-beginning-of-data)
    (setq beg (point))
    (end-of-buffer)
    (setq end (point)))
  (delete-matching-lines (concat "^| " data-mode-current-highlight-number) beg end))



                                              











;; 
;; MARK BY CALCULATING WITH NUMBERS
;; 


(defun mark-region-by-calculation (prefix beg end calculation)
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
  (interactive "P\nr\nsFilter: ")
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
     (when (not (use-region-p))
       (goto-beginning-of-data)
       (setq beg (point))
       (end-of-buffer)
       (setq end (point)))
    (let ((variables '()))
      (let ((start-pos 0))
        (while (string-match "$\\([0-9]*\\)" calculation start-pos)
          (push (string-to-number (match-string 1 calculation)) variables)
          (setq start-pos (match-end 0))))
      (setq results (apply-defun-to-region-lines beg end stop-marking-after-errors 'mark-line-by-calculation calculation variables prefix))
      (message "Marked: %s lines, Visited: %s, Skipped: %s, Errors: %s %s"
                (nth 0 results)
                (nth 1 results)
                (nth 2 results)
                (nth 3 results)
                (if (nth 4 results) "    --- STOPPED evaluating after reaching error limit" "")))))
  



;; TODO: clear out variables afterwards...
(defun mark-line-by-calculation (calculation variables prefix)
  (setq values (split-string
                (buffer-substring-no-properties
                 (line-beginning-position)
                 (line-end-position)) "|" t))
 (setq values (cons "" values))
 (setq line-calculation calculation)
 (catch 'line-should-be-ignored
   (mapc (lambda (arg)
           (when (or (not (nth arg values))
                   (string-match ignore-columns-regexp (nth arg values)))               
             (when prefix (message "Skipping line %s" values))
             (throw 'line-should-be-ignored -2))
           (setq line-calculation
                 (replace-regexp-in-string
                  (concat "$" (number-to-string arg)) (nth arg values)
                  line-calculation)))
         variables)
   (setq result (ignore-errors (calc-eval line-calculation)))   
   (when prefix
     (message "Values: %s\nVariables: %s\nFormula: %s\nLine calculation: %s (Result: %s)"
              values variables calculation line-calculation result))
   (cond ((or (listp result) (consp result) (not result))
          (log-error (format "Error in formula %s" line-calculation) buffer-file-name (line-number-at-pos)) -1)
         ((string= "1" result)
          (progn
            (mark-current-line-or-lines-in-region  (line-beginning-position) (line-end-position))
            1))
         (t 0))))



    



;; Format the table

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


;; Motion and Marking Functions

(defun goto-beginning-of-data ()
  "Move point to the first character of the first line in the data table. If the table has a header with +---+---+, point will be on the line after +---+---+."
  (interactive)
  (beginning-of-buffer)
  (forward-line)
  (if (string= "+-" (buffer-substring-no-properties (line-beginning-position) (+ (line-beginning-position) 2)))
      (forward-line)
    (forward-line -1)))

(defun goto-end-of-data ()
  "Move point to the first character of the first line in the data table. If the table has a header with +---+---+, point will be on the line after +---+---+."
  (interactive)
  (end-of-buffer)
  (search-backward-regexp "^| ")
  (end-of-line))


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
- -3 if apply-defun-to-region-lines should NOT move to the next line
"
  (message "X Beg %s end %s" beg end)
    
  (save-excursion
    (let ((num-lines-visited 0)
          (num-acted-on-by-myfunc 0)
          (num-errors 0)
          (num-skipped 0))
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



;; Mode definition
(define-derived-mode data-mode nil "Data Mode"
  "Major mode for editing rows/columns of data"
  (toggle-truncate-lines 1)
  (setq inhibit-field-text-motion t)
  (add-pipes-before-after)
  (add-header-delimiter))

(define-key data-mode-map (kbd "m") 'mark-current-line-or-lines-in-region)
(define-key data-mode-map (kbd "u") 'unmark-current-line-or-lines-in-region)
(define-key data-mode-map (kbd "U") 'unmark-all-lines)
(define-key data-mode-map (kbd "h") 'mark-region)
(define-key data-mode-map (kbd "e") 'unmark-region)

(define-key data-mode-map (kbd "d") 'delete-marked-lines)



(define-key data-mode-map (kbd "+") 'summarize-column)
(define-key data-mode-map (kbd "^") 'sort-column)

(define-key data-mode-map (kbd "c") 'mark-region-by-calculation)




(define-key data-mode-map (kbd "1") (lambda () (interactive) (set-highlight-color "1")))
(define-key data-mode-map (kbd "2") (lambda () (interactive) (set-highlight-color "2")))
(define-key data-mode-map (kbd "3") (lambda () (interactive) (set-highlight-color "3")))



(provide 'data-mode)



 
