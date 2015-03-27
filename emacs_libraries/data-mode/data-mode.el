;;; data-mode.el --- Data buffer summary!
;; todo
;; X same: delete
;; - column totals in formulas
;; - previous row in formulas
;; - variable ranges in formulas (columns 1- 4)
;; x overlay with column numbers
;; - info-at-point (which column)
;; X handle column headers
;; x fix region or this line
;; - histogram function
;; x regexp in addition to calculation
;; - only act on already marked lines with second pass of marking
;; x Import a table from SQL
;; x Goto top / bottom of current column (t, b)
;; - Keep overlays when moving lines


;; ----------------------------------------------------------------------------------------------------
;;
;; Variables and Color Definitions
;;
;; ----------------------------------------------------------------------------------------------------

;;; Commentary:
;; 

;;; Code:

(defvar data-mode-current-highlight-number "1" "The number of the current highlight face.")
(defvar data-mode-current-highlight-face 'data-mode-highlight-1 "The face that will be used for the next highlighting operation.")
(defvar stop-marking-after-errors 10 "After this many calculation errors, stop doing mark-line-or-region.")
(defvar ignore-columns-regexp ".*\\?.*" "Ignore any column value that matches this regular expression.  Applies to filtering by calculation, etc.")
(defvar last-sort-column 0 "The last column that was sorted by the sort function.")
(defvar last-sort-order nil "nil = ascending, t = descending.")



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
       "Yellow face for highlighting (default)."
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
       "Purple face for highlighting."
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
       "Red face for highlighting."
       :group 'data-mode-faces)


(defun set-highlight-color (arg)
  "Set the current highlighting color for data-mode. The color number selected will be used for highlighting and marking.
Argument ARG The number of the color; see data-mode-highlight-ARG for color definitions."
  (setq data-mode-current-highlight-number arg)
  (setq data-mode-current-highlight-face (concat "data-mode-highlight-" arg)))



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
    
(defun bottom-of-column ()
  (interactive)
  (let ((col (current-column)))
    (goto-end-of-data)
    (beginning-of-line)
    (forward-char col)))

(defun next-field (count)
  "Move to the next field. Wrap to the beginning of the next line if at the last field. With numeric prefix, move COUNT fields forward."
  (interactive "p")
  (when (not (search-forward "|" nil t count))
    (forward-line)
    (beginning-of-line)))

(defun previous-field (count)
  "Move to the previous field. Wrap to the end of the previous line if at the last field. With numeric prefix, move COUNT fields backward."
  (interactive "p")
  (if (not (search-backward "|" nil t (if count (+ count 1) 2)))
      (progn
        (forward-line -1)
        (end-of-line))
      (forward-char)))

(defun forward-page ()
  "Move forward a 'page' (5 columns)."
  (interactive)
  (next-field 7))

(defun backward-page ()
  "Move forward a 'page' (5 columns)."
  (interactive)
  (next-field 7))

(defun set-highlight-color-1 ()
  (interactive)
  (set-highlight-color "1"))

(defun set-highlight-color-2 ()
  (interactive)
  (set-highlight-color "2"))

(defun set-highlight-color-3 ()
  (interactive)
  (set-highlight-color "3"))

(defun mark-lines (beg end)
  (interactive "r")
  (save-excursion
    (fix-region beg end)
    (mark-lines-in-region (mark) (point))))

(defun unmark-lines (beg end)
  (interactive "r")
  (save-excursion
    (fix-region beg end)
    (unmark-lines-in-region (mark) (point))))

(defun unmark-all-lines ()
  (interactive)
  (save-excursion
    (mark-whole-buffer)
    (fix-region (region-beginning) (region-end))
    (unmark-lines-in-region (mark) (point))))


(defun delete-marked-lines (beg end)
  "Kill lines that are marked."
  (interactive "r")
  (save-excursion
    (fix-region beg end)
    (delete-marked-lines-in-region (mark) (point))))
  

(defun delete-all-marked-lines ()
  (interactive)
  (save-excursion
    (mark-whole-buffer)
    (fix-region (region-beginning) (region-end))
    (delete-marked-lines-in-region (mark) (point))))


(defun summarize-column ()
  "With point inside a column, calculate metrics and display in the mini buffer.
Metrics: sum, count, avg., min, max"
  (interactive)
  (save-excursion
    (select-rectangle-of-current-column)
    (let ((sum 0) (max most-negative-fixnum) (min most-positive-fixnum) (count 0) (number 0))
          (mapc (lambda (line)
                  (setq number (make-number line))
                  (setq sum (+ sum number))
                  (if (> number max) (setq max number))
                  (if (< number min) (setq min number))
                  (setq count (+ count 1)))
          (extract-rectangle (region-beginning) (region-end)))
      (message "Sum: %s, Min: %s, Max: %s, Avg: %s, Count: %s" sum min max (/ sum count) count))))



(defun sort-column ()
  "With point inside a column, sort the column using `sort-columns'.  Sort again to flip the order."
  (interactive)
  (let ((mycolumn (current-column))
        (this-sort-column 0))
    (save-excursion
      (setq this-sort-column (select-rectangle-of-current-column))
      (sort-columns
       (cond ((= last-sort-column this-sort-column) (not last-sort-order))
             (t nil))
       (region-beginning) (region-end)))
    (setq last-sort-column this-sort-column)
    (setq last-sort-order (not last-sort-order))
    (move-to-column mycolumn)))

(defun mark-lines-by-regexp (regexp)
  "Search for regular expression in the current column and mark any lines that match with the current marker color."
  (interactive "sRegular expression (escape literals with \): ")
  (save-excursion
    (let* ((this-sort-column (select-rectangle-of-current-column))
           (iter-count 1)
           (tmp-regexp "|"))
      (while (< iter-count this-sort-column)
        (setq iter-count (1+ iter-count))
        (setq tmp-regexp (concat tmp-regexp "[^|]*|")))
      (setq tmp-regexp (concat tmp-regexp " *" regexp ".*"))
      (goto-beginning-of-data)
      (while (re-search-forward tmp-regexp nil t)
        (mark-lines-in-region  (line-beginning-position) (line-end-position))))))


(defun mark-lines-by-calculation (prefix beg end calculation)
  "Evaluate the CALCULATION on all lines in the region BEG END, and highlight all lines where CALCULATION is true. If no region, operate on the entire buffer.

CALCULATION is any valid GNU Calc calculation, using column variables to refer to the data on the current line. Column variables are of the form $X, where X is the number of the column, with the leftmost column being 1. If a variable is not given in CALCULATION, determine the column where point is currently, and prepend it to CALCULATION.

Examples:
1) Mark all lines where column 7 is at least 20% greater than column 8

   $7 >= (1.2*$8)

2) Mark all lines where the current column is less than 10

   < 10

   In this example, since no column variable is referenced, the calculation will be modified before evaluating. Assuming point is in column 12:

   $12 < 10"
  (interactive "P\nr\nsFilter: ")
  (message "Evaluating... (C-g to quit)")
  (save-excursion
    (if (not (string-match "$[0-9]*" calculation))
        (setq calculation
              (concat "$"
                      (number-to-string (count-matches "|"  (line-beginning-position) (point) nil))
                      " "
                      calculation)))
    (if (not (use-region-p)) (mark-whole-buffer))
    (fix-region (region-beginning) (region-end))
    (mark-lines-by-calculation-in-region prefix (mark) (point) calculation)))
    




;; ----------------------------------------------------------------------------------------------------
;;
;; Mid-level functions - operate on all selected lines
;;
;; ----------------------------------------------------------------------------------------------------

(defun unmark-lines-in-region (beg end)
  "Remove all data-mode faces from the lines in the region, or if there's no active region, the current line. When applying it to the region, widen the selection to include complete lines."
  (message "Beg %s end %s" beg end)
  (fix-region beg end)
  (message "Beg %s end %s" (region-beginning) (region-end) )
  
  (apply-defun-to-region-lines (region-beginning) (region-end) 100
         '(lambda ()
            (unmark-region (line-beginning-position) (line-end-position))
            (save-excursion
              (beginning-of-line)
              (forward-char 2)
              (delete-char 1)
              (insert " "))
            0)))
    

(defun mark-lines-in-region (beg end)
  "Apply the currently selected highlighting face to the lines in the region, or if there's no active region, the current line. When applying it to the region, widen the selection to include complete lines."
  (apply-defun-to-region-lines beg end 100
         '(lambda ()
            (mark-region (line-beginning-position) (line-end-position))
            (save-excursion (beginning-of-line)
                            (forward-char 2)
                            (delete-char 1)
                            (insert data-mode-current-highlight-number))
            0)))


(defun mark-lines-by-calculation-in-region (prefix beg end calculation)
  (ignore-errors (kill-buffer "*data-mode results*"))
  (save-excursion
    (let ((variables '())
          (results nil))
      (let ((start-pos 0))
        (while (string-match "$\\([0-9]*\\)" calculation start-pos)
          (push (string-to-number (match-string 1 calculation)) variables)
          (setq start-pos (match-end 0))))
      (setq results (apply-defun-to-region-lines beg end stop-marking-after-errors 'calculate calculation variables prefix))
      (message "Marked: %s lines, Visited: %s, Skipped: %s, Errors: %s %s"
                (nth 0 results)
                (nth 1 results)
                (nth 2 results)
                (nth 3 results)
                (if (nth 4 results) "    --- STOPPED evaluating after reaching error limit" "")))))
  







;; ----------------------------------------------------------------------------------------------------
;;
;; Low-level functions
;;
;; ----------------------------------------------------------------------------------------------------


(defun calculate (calculation variables prefix)
  (let* ((values (split-string
                (buffer-substring-no-properties
                 (line-beginning-position)
                 (line-end-position)) "|" t))
         (values (cons "" values))
         (line-calculation calculation))
    (catch 'line-should-be-ignored
      (mapc (lambda (arg)
              (when (or (not (nth arg values))
                        (string-match ignore-columns-regexp (nth arg values)))
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
               (mark-lines-in-region  (line-beginning-position) (line-end-position))
               1))
            (t 0)))))


(defun delete-marked-lines-in-region (beg end)
  (delete-matching-lines (concat "^| " data-mode-current-highlight-number) beg end))

(defun unmark-region (beg end)
  "Remove all data-mode faces from the region."
  (let ((face-name nil))
    (mapcar (lambda (arg)
              (if (eq (type-of (overlay-get arg 'face)) 'symbol)
                  (setq face-name (symbol-name (overlay-get arg 'face)))
                (setq face-name (overlay-get arg 'face)))
              (if (string-match "data-mode" face-name)
                  (delete-overlay arg)))
            (overlays-in beg end ))))

(defun mark-region (beg end)
  "Apply the currently selected highlighting face to the region. Use to mark characters or strings (parts of lines).
If region is already marked by a different highlight, replace the old highlight."
  (unmark-region beg end)
  (overlay-put (make-overlay beg end) 'face  data-mode-current-highlight-face))

(defun goto-beginning-of-data ()
  "Move point to the first character of the first line in the data table.  If the table has a header with +---+---+, point will be on the line after +---+---+."
  (interactive)
  (beginning-of-buffer)
  (forward-line 3))

(defun goto-end-of-data ()
  "Move point to the first character of the first line in the data table.  If the table has a header with +---+---+, point will be on the line after +---+---+."
  (interactive)
  (end-of-buffer)
  (search-backward-regexp "^| ")
  (end-of-line))


(defun fix-region (beg end)
  "Ensure that the region only includes data lines, and that the (mark) is at the first character of the first line in the region, and that (point) is at the last character of the last line in the region.

With no active region, mark the current line"
  (interactive "r")
  (if (not (use-region-p))
      (progn
        (beginning-of-line)
        (set-mark (point))
        (end-of-line))
    (progn
      (goto-beginning-of-data)
      (when (< (point) beg)
        (goto-char beg))
      (beginning-of-line)
      (set-mark (point))
      (goto-end-of-data)
      (when (> (point) end)
        (goto-char end))
      (end-of-line)
      )))

(defun select-rectangle-of-current-column ()
  "Return the column number"
  (interactive)
  (let ((column-start 0)
        (column-end 0)
        (column-number 0))
    (search-backward "|" nil)
    (forward-char)
    (setq column-start (current-column))
    (search-forward "|" nil)
    (backward-char)
    (setq column-end (current-column))
    (goto-end-of-data)
    (move-to-column column-end)
    (set-mark (point))
    (goto-beginning-of-data)
    (move-to-column column-start)
    (count-matches "|" (line-beginning-position) (point))))


(defun make-number (n)
      "Turn a string into a number, being tolerant of commas and even other
'junk'."
    (while (string-match "[^-0-9.]" n)
      (setq n (replace-match "" nil nil n)))
      (string-to-number n))


(defun is-current-line-marked ()
  (let ((ismarked nil))
    (let ((face-name nil))
      (mapc (lambda (arg)
              (setq face-name nil)
              (if (eq (type-of (overlay-get arg 'face)) 'symbol)
                  (setq face-name (symbol-name (overlay-get arg 'face)))
                (setq face-name (overlay-get arg 'face)))
              (when (string-match data-mode-current-highlight-face face-name)
                (setq ismarked t)))
            (overlays-in (line-beginning-position) (line-end-position)))
      ismarked)))

    


                                              







;; ----------------------------------------------------------------------------------------------------
;;
;; Table creation and formatting functions
;;
;; ----------------------------------------------------------------------------------------------------



(defun format-table ()
  (interactive)
  "Format the table per `data-mode' conventions:
- Columns are encolosed in | |
- The second line is a separator (+-----+-----+ etc.)
- The first column is a special marker column (|    |)
- There is no whitespace after the end of the table"
  (save-excursion
    (beginning-of-buffer)  
    (flush-lines "^[ ]*$")
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


(defvar data-mode-keymap nil "Keymap for `data-mode'.")
(when (not data-mode-keymap)
  (setq data-mode-keymap (make-sparse-keymap))
  (define-key data-mode-keymap (kbd "s") 'sort-column)
  (define-key data-mode-keymap (kbd "+") 'summarize-column)
  (define-key data-mode-keymap (kbd "1") 'set-highlight-color-1)
  (define-key data-mode-keymap (kbd "2") 'set-highlight-color-2)
  (define-key data-mode-keymap (kbd "3") 'set-highlight-color-3)

  (define-key data-mode-keymap (kbd "t") 'top-of-column)
  (define-key data-mode-keymap (kbd "b") 'bottom-of-column)
  
  (define-key data-mode-keymap (kbd "m") 'mark-lines)
  (define-key data-mode-keymap (kbd "u") 'unmark-lines)
  (define-key data-mode-keymap (kbd "U") 'unmark-all-lines)
  (define-key data-mode-keymap (kbd "c") 'mark-lines-by-calculation)
  (define-key data-mode-keymap (kbd "r") 'mark-lines-by-regexp)
  
  (define-key data-mode-keymap (kbd "d") 'delete-marked-lines)
  (define-key data-mode-keymap (kbd "D") 'delete-all-marked-lines)
  (define-key data-mode-keymap (kbd "TAB") 'next-field)
  (define-key data-mode-keymap (kbd "<S-tab>") 'previous-field)
  (define-key data-mode-keymap (kbd "<C-tab>") 'forward-page)
  (define-key data-mode-keymap (kbd "<S-C-tab>") 'backward-page))
  



;; Mode definition
(define-derived-mode data-mode nil "Data Mode"
  "Major mode for editing rows/columns of data"
  (interactive)
  (kill-all-local-variables)
  (toggle-truncate-lines 1)
  (setq inhibit-field-text-motion 1)
  (format-table)
  (setq major-mode 'data-mode)
  (setq mode-name "Data Mode X")
  (use-local-map data-mode-keymap)
  (run-hooks 'data-mode-hooks))


(provide 'data-mode)


;;; data-mode.el ends here
