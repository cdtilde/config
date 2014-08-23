
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;;  BARE MINIMUM STUFF
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message "")
(setq initial-scratch-message "")
(defun startup-echo-area-message ()
  (message ""))
 
(setq ring-bell-function 'ignore)
(setq query-replace-highlight t)
(fset 'yes-or-no-p 'y-or-n-p)
(blink-cursor-mode 0)
(setq vc-follow-symlinks t)
(savehist-mode 1)

;; Backup files
(setq make-backup-files t)
(setq version-control t)
(setq backup-directory-alist (quote ((".*" . "~/.emacs_backups/"))))
(setq delete-old-versions t)

;; Mark and parentheses
;; With region active, type to delete entire region
(delete-selection-mode 1)
(transient-mark-mode t)
(column-number-mode 1)
(show-paren-mode t)
(setq show-paren-style 'mixed)

;; OS X options
(setq mac-option-modifier 'meta)
(setq ns-use-native-fullscreen nil)  ;; Don't put Emacs in a separate space

;; (if (display-graphic-p)
;;     (progn
;;       (fringe-mode 0)
;;       (tool-bar-mode 0)
;;       (scroll-bar-mode 0)
;;       (menu-bar-mode 1))
;;   (progn
;;     (tool-bar-mode 0)
;;     (scroll-bar-mode 0)
;;     (fringe-mode 0)
;;     (menu-bar-mode 0)))


(tool-bar-mode 0)
(scroll-bar-mode 0)
(fringe-mode 0)
(menu-bar-mode 0)



(setq scroll-step 1)
(setq scroll-conservatively 9999999)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;;  SQL 
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "~/.dotfiles/emacs_libraries/sql-tools")
(require 'sql-tools)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;;  LOOK
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'custom-theme-load-path "~/.dotfiles/emacs_libraries/themes")

(defun cousine-font()  
  (interactive)
  (progn
    (set-default-font "-*-Cousine-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1")
    (add-to-list 'default-frame-alist '
                 (font . "-*-Cousine-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1"))
    ))


(defun iawriter()
  (interactive)
  (load-theme 'iawriter t))

(defun wombat()
  (interactive)
  (load-theme 'wombat2 t))


(wombat)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;;  WINDOWS
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun rotate-windows ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))


(defun flip-windows ()
  "Rotate your windows"
  (interactive)
  (cond ((not (> (count-windows)1))
         (message "You can't rotate a single window!"))
        (t
         (setq i 1)
         (setq numWindows (count-windows))
         (while  (< i numWindows)
           (let* (
                  (w1 (elt (window-list) i))
                  (w2 (elt (window-list) (+ (% i numWindows) 1)))

                  (b1 (window-buffer w1))
                  (b2 (window-buffer w2))

                  (s1 (window-start w1))
                  (s2 (window-start w2))
                  )
             (set-window-buffer w1  b2)
             (set-window-buffer w2 b1)
             (set-window-start w1 s2)
             (set-window-start w2 s1)
             (setq i (1+ i)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;;  INDENTATION
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(setq-default indent-tabs-mode nil)
(setq tab-width 2)
;; Convert from tabs to spaces: untabify (reverse with tabify)

(add-hook 'java-mode-hook (lambda ()
                            (setq c-basic-offset 2)))

(add-hook 'sql-mode-hook (lambda ()
                           (setq c-basic-offset 2)                           ))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;;  MOUSE SCROLLING
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Turn on horizontal scrolling with mouse wheel
;(global-set-key [wheel-right] 'scroll-left)
;(global-set-key [wheel-left]  'scroll-right)

(global-set-key (kbd "<wheel-right>")  (lambda ()(interactive) (scroll-left 2)))
(global-set-key (kbd "<wheel-left>")  (lambda ()(interactive) (scroll-right 2)))

(put 'scroll-left 'disabled nil)

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;;  MISC
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun file-info ()
  (interactive)
  (if (not (buffer-file-name))
      (message "Buffer does not have a file: line %d of %d"
               (line-number-at-pos)
               (count-lines (point-min) (point-max)))
    (progn 
      (let ((file-attributes (file-attributes (buffer-file-name) 'string)))
        (let ((size (nth 7 file-attributes)))
          (message "\"%s\": line %d of %d (%d bytes) %s"
                   (buffer-file-name)
                   (line-number-at-pos)
                   (count-lines (point-min) (point-max))
                   size
                   (if (file-writable-p (buffer-file-name)) "Read/Write" "Read Only")))))))


(defun enable-whitespace-marking()
  (interactive)
  (require 'whitespace)
  (setq whitespace-style '(face lines-tail))
  (global-whitespace-mode t))


;;Behave like vi's o command
(defun open-next-line (arg)
  "Move to the next line and then opens a line.
    See also `newline-and-indent'."
  (interactive "p")
  (end-of-line)
  (open-line arg)
  (next-line 1))


(defun open-previous-line (arg)
      "Open a new line before the current one. 
     See also `newline-and-indent'."
      (interactive "p")
      (beginning-of-line)
      (open-line arg))

(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
	((looking-at "\\s\)") (forward-char 1) (backward-list 1))
	            (t (self-insert-command (or arg 1)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;;  ORG-MODE
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Required to syntax-color code when exporting to Latex
;; Requires Pygmentize. To install: sudo easy_install Pygments
(require 'ox-latex)
(add-to-list 'org-latex-packages-alist '("" "minted"))
(setq org-latex-listings 'minted)
(setq org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))


(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (sh . t)
   (calc . t)
   (sql . t)
   (R . t)
   (emacs-lisp . t)))

(setq org-confirm-babel-evaluate nil)
;;(require 'ess-site)
(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match that used by the user's shell.
This is particularly useful under Mac OSX, where GUI apps are not started from a shell."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(set-exec-path-from-shell-PATH)

(defun cf (calcarg formatarg &optional thousands &optional dollar)
  "Calcualtes the formulat calcarg and formats the result using using the printf formatarg format. In addition to formatarg, cf will optionally add a thousand separator (,) and a dollar sign."
  (interactive)
  (message "Calcarg = %s, formatarg = %s, thousands=%s, dollar=%s" calcarg formatarg thousands dollar)
  (let ((c (string-to-number (calc-eval calcarg))))
    (message "C = %d" c)
    (concat (if dollar "$")
            (if thousands
                (calc-eval '("$" calc-group-digits t) 'num (truncate c))
              (number-to-string (truncate c)))
            (substring (format formatarg (- c (truncate c))) 1 nil))))

(defun currency-format(formatarg arg)
  (concat "$"
          (calc-eval '("$" calc-group-digits t) 'num (truncate arg))
          (substring (format formatarg (- arg (truncate arg))) 1 nil)))

(defun de-currency(arg)
  (setq arg (replace-regexp-in-string "\\$" "" arg))
  (setq arg (replace-regexp-in-string "," "" arg)))    

(add-hook 'org-mode-hook (lambda ()
                           (progn

                           ;; Modified version to allow $ signs and thousands separators in column values
                           (defun org-table-eval-formula (&optional arg equation
                                                                    suppress-align suppress-const
                                                                    suppress-store suppress-analysis)
                             "Replace the table field value at the cursor by the result of a calculation.

This function makes use of Dave Gillespie's Calc package, in my view the
most exciting program ever written for GNU Emacs.  So you need to have Calc
installed in order to use this function.

In a table, this command replaces the value in the current field with the
result of a formula.  It also installs the formula as the \"current\" column
formula, by storing it in a special line below the table.  When called
with a `C-u' prefix, the current field must be a named field, and the
formula is installed as valid in only this specific field.

When called with two `C-u' prefixes, insert the active equation
for the field back into the current field, so that it can be
edited there.  This is useful in order to use \\[org-table-show-reference]
to check the referenced fields.

When called, the command first prompts for a formula, which is read in
the minibuffer.  Previously entered formulas are available through the
history list, and the last used formula is offered as a default.
These stored formulas are adapted correctly when moving, inserting, or
deleting columns with the corresponding commands.

The formula can be any algebraic expression understood by the Calc package.
For details, see the Org-mode manual.

This function can also be called from Lisp programs and offers
additional arguments: EQUATION can be the formula to apply.  If this
argument is given, the user will not be prompted.  SUPPRESS-ALIGN is
used to speed-up recursive calls by by-passing unnecessary aligns.
SUPPRESS-CONST suppresses the interpretation of constants in the
formula, assuming that this has been done already outside the function.
SUPPRESS-STORE means the formula should not be stored, either because
it is already stored, or because it is a modified equation that should
not overwrite the stored one."
                             (interactive "P")
                             (org-table-check-inside-data-field)
                             (or suppress-analysis (org-table-get-specials))
                             (if (equal arg '(16))
                                 (let ((eq (org-table-current-field-formula)))
                                   (or eq (user-error "No equation active for current field"))
                                   (org-table-get-field nil eq)
                                   (org-table-align)
                                   (setq org-table-may-need-update t))
                               (let* (fields
                                      (ndown (if (integerp arg) arg 1))
                                      (org-table-automatic-realign nil)
                                      (case-fold-search nil)
                                      (down (> ndown 1))
                                      (formula (if (and equation suppress-store)
                                                   equation
                                                 (org-table-get-formula equation (equal arg '(4)))))
                                      (n0 (org-table-current-column))
                                      (org-tbl-calc-modes (copy-sequence org-calc-default-modes))
                                      (numbers nil) ; was a variable, now fixed default
                                      (keep-empty nil)
                                      n form form0 formrpl formrg bw fmt x ev orig c lispp literal
                                      duration duration-output-format currency)
                                 ;; Changed previous line - initialized currency
                                 ;; Parse the format string.  Since we have a lot of modes, this is
                                 ;; a lot of work.  However, I think calc still uses most of the time.
                                 (if (string-match ";" formula)
                                     (let ((tmp (org-split-string formula ";")))
                                       (setq formula (car tmp)
                                             fmt (concat (cdr (assoc "%" org-table-local-parameters))
                                                         (nth 1 tmp)))
                                       (while (string-match "\\([pnfse]\\)\\(-?[0-9]+\\)" fmt)
                                         (setq c (string-to-char (match-string 1 fmt))
                                               n (string-to-number (match-string 2 fmt)))
                                         (if (= c ?p)
                                             (setq org-tbl-calc-modes (org-set-calc-mode 'calc-internal-prec n))
                                           (setq org-tbl-calc-modes
                                                 (org-set-calc-mode
                                                  'calc-float-format
                                                  (list (cdr (assoc c '((?n . float) (?f . fix)
                                                                        (?s . sci) (?e . eng))))
                                                        n))))
                                         (setq fmt (replace-match "" t t fmt)))
                                       (if (string-match "T" fmt)
                                           (setq duration t numbers t
                                                 duration-output-format nil
                                                 fmt (replace-match "" t t fmt)))
                                       (if (string-match "t" fmt)
                                           (setq duration t
                                                 duration-output-format org-table-duration-custom-format
                                                 numbers t
                                                 fmt (replace-match "" t t fmt)))
                                       (if (string-match "N" fmt)
                                           (setq numbers t
                                                 fmt (replace-match "" t t fmt)))
                                       (if (string-match "L" fmt)
                                           (setq literal t
                                                 fmt (replace-match "" t t fmt)))
                                       ;; Start Change -- format specified of "Currency"
                                       (if (string-match "C" fmt)
                                           (setq currency t
                                                 fmt (replace-match "" t t fmt)))
                                       ;; End Change
                                       (if (string-match "E" fmt)
                                           (setq keep-empty t
                                                 fmt (replace-match "" t t fmt)))
                                       (while (string-match "[DRFS]" fmt)
                                         (setq org-tbl-calc-modes (org-set-calc-mode (match-string 0 fmt)))
                                         (setq fmt (replace-match "" t t fmt)))
                                       (unless (string-match "\\S-" fmt)
                                         (setq fmt nil))))
                                 (if (and (not suppress-const) org-table-formula-use-constants)
                                     (setq formula (org-table-formula-substitute-names formula)))
                                 (setq orig (or (get-text-property 1 :orig-formula formula) "?"))
                                 (while (> ndown 0)
                                   (setq fields (org-split-string
                                                 (buffer-substring-no-properties (point-at-bol) (point-at-eol))
                                                 " *| *"))
                                   ;; replace fields with duration values if relevant
                                   (if duration
                                       (setq fields
                                             (mapcar (lambda (x) (org-table-time-string-to-seconds x))
                                                     fields)))
                                   (if (eq numbers t)
                                       (setq fields (mapcar
                                                     (lambda (x)
                                                       (if (string-match "\\S-" x)
                                                           (number-to-string (string-to-number x))
                                                         x))
                                                     fields)))
                                   (setq ndown (1- ndown))
                                   (setq form (copy-sequence formula)
                                         lispp (and (> (length form) 2) (equal (substring form 0 2) "'(")))
                                   (if (and lispp literal) (setq lispp 'literal))

                                   ;; Insert row and column number of formula result field
                                   (while (string-match "[@$]#" form)
                                     (setq form
                                           (replace-match
                                            (format "%d"
                                                    (save-match-data
                                                      (if (equal (substring form (match-beginning 0)
                                                                            (1+ (match-beginning 0)))
                                                                 "@")
                                                          (org-table-current-dline)
                                                        (org-table-current-column))))
                                            t t form)))

                                   ;; Check for old vertical references
                                   (setq form (org-table-rewrite-old-row-references form))
                                   ;; Insert remote references
                                   (while (string-match "\\<remote([ \t]*\\([-_a-zA-Z0-9]+\\)[ \t]*,[ \t]*\\([^\n)]+\\))" form)
                                     (setq form
                                           (replace-match
                                            (save-match-data
                                              (org-table-make-reference
                                               (let ((rmtrng (org-table-get-remote-range
                                                              (match-string 1 form) (match-string 2 form))))
                                                 (if duration
                                                     (if (listp rmtrng)
                                                         (mapcar (lambda(x) (org-table-time-string-to-seconds x)) rmtrng)
                                                       (org-table-time-string-to-seconds rmtrng))
                                                   rmtrng))
                                               keep-empty numbers lispp))
                                            t t form)))
                                   ;; Insert complex ranges
                                   (while (and (string-match org-table-range-regexp form)
                                               (> (length (match-string 0 form)) 1))
                                     (message "Match is %s" (match-string 0 form))
                                     (setq formrg (save-match-data
                                                    (org-table-get-range (match-string 0 form) nil n0)))
                                     (message "formrg is %s" formrg)
                                     (setq formrpl
                                           (save-match-data
                                             (org-table-make-reference
                                              ;; possibly handle durations
                                              (if duration
                                                  (if (listp formrg)
                                                      (mapcar (lambda(x) (org-table-time-string-to-seconds x)) formrg)
                                                    (org-table-time-string-to-seconds formrg))
                                                ;; Begin Change - handle vectors from range specifications 
                                                (if (and (listp formrg) currency)
                                                    (mapcar (lambda(x) (de-currency x)) formrg)
                                                  formrg))
                                              ;; End Change
                                              keep-empty numbers lispp)))
                                     (message "After, formrepl = %s" formrpl)
                                     (if (not (save-match-data
                                                (string-match (regexp-quote form) formrpl)))
                                         (setq form (replace-match formrpl t t form))
                                       (user-error "Spreadsheet error: invalid reference \"%s\"" form)))
                                   ;; Insert simple ranges
                                   (while (string-match "\\$\\([0-9]+\\)\\.\\.\\$\\([0-9]+\\)"  form)
                                     (setq form
                                           (replace-match
                                            (save-match-data
                                              (org-table-make-reference
                                               (org-sublist
                                                fields (string-to-number (match-string 1 form))
                                                (string-to-number (match-string 2 form)))
                                               keep-empty numbers lispp))
                                            t t form)))
                                   (setq form0 form)
                                   ;; Insert the references to fields in same row
                                   (while (string-match "\\$\\(\\([-+]\\)?[0-9]+\\)" form)
                                     (message (match-string 1 form))
                                     (setq n (+ (string-to-number (match-string 1 form))
                                                (if (match-end 2) n0 0))
                                           x (nth (1- (if (= n 0) n0 (max n 1))) fields))
                                     ;; Begin change - eliminate $ and , from the input value
                                     (when (and currency x)
                                       (message "Second thing")
                                       (setq x (de-currency x)))
                                     ;; End Change
                                     (unless x (user-error "Invalid field specifier \"%s\""
                                                           (match-string 0 form)))
                                     (setq form (replace-match
                                                 (save-match-data
                                                   (org-table-make-reference
                                                    x keep-empty numbers lispp))
                                                 t t form)))

                                   (if lispp
                                       (setq ev (condition-case nil
                                                    (eval (eval (read form)))
                                                  (error "#ERROR"))
                                             ev (if (numberp ev) (number-to-string ev) ev)
                                             ev (if duration (org-table-time-seconds-to-string
                                                              (string-to-number ev)
                                                              duration-output-format) ev))
                                     (or (fboundp 'calc-eval)
                                         (user-error "Calc does not seem to be installed, and is needed to evaluate the formula"))
                                     ;; Use <...> time-stamps so that Calc can handle them
                                     (setq form (replace-regexp-in-string org-ts-regexp3 "<\\1>" form))
                                     ;; I18n-ize local time-stamps by setting (system-time-locale "C")
                                     (when (string-match org-ts-regexp2 form)
                                       (let* ((ts (match-string 0 form))
                                              (tsp (apply 'encode-time (save-match-data (org-parse-time-string ts))))
                                              (system-time-locale "C")
                                              (tf (or (and (save-match-data (string-match "[0-9]\\{1,2\\}:[0-9]\\{2\\}" ts))
                                                           (cdr org-time-stamp-formats))
                                                      (car org-time-stamp-formats))))
                                         (setq form (replace-match (format-time-string tf tsp) t t form))))

                                     (setq ev (if (and duration (string-match "^[0-9]+:[0-9]+\\(?::[0-9]+\\)?$" form))
                                                  form
                                                (calc-eval (cons form org-tbl-calc-modes)
                                                           (when (and (not keep-empty) numbers) 'num)))
                                           ev (if duration (org-table-time-seconds-to-string
                                                            (if (string-match "^[0-9]+:[0-9]+\\(?::[0-9]+\\)?$" ev)
                                                                (string-to-number (org-table-time-string-to-seconds ev))
                                                              (string-to-number ev))
                                                            duration-output-format)
                                                ev)))

                                   (when org-table-formula-debug
                                     (with-output-to-temp-buffer "*Substitution History*"
                                       (princ (format "Substitution history of formula
Orig:   %s
$xyz->  %s
@r$c->  %s
$1->    %s\n" orig formula form0 form))
                                       (if (listp ev)
                                           (princ (format "        %s^\nError:  %s"
                                                          (make-string (car ev) ?\-) (nth 1 ev)))
                                         (princ (format "Result: %s\nFormat: %s\nFinal:  %s"
                                                        ev (or fmt "NONE")
                                                        (if fmt (currency-format fmt (string-to-number ev)) ev)))))
                                     (setq bw (get-buffer-window "*Substitution History*"))
                                     (org-fit-window-to-buffer bw)
                                     (unless (and (org-called-interactively-p 'any) (not ndown))
                                       (unless (let (inhibit-redisplay)
                                                 (y-or-n-p "Debugging Formula.  Continue to next? "))
                                         (org-table-align)
                                         (user-error "Abort"))
                                       (delete-window bw)
                                       (message "")))
                                   (if (listp ev) (setq fmt nil ev "#ERROR"))
                                   (org-table-justify-field-maybe
                                    (format org-table-formula-field-format
                                            (if fmt (currency-format fmt (string-to-number ev)) ev)))
                                   (if (and down (> ndown 0) (looking-at ".*\n[ \t]*|[^-]"))
                                       (call-interactively 'org-return)
                                     (setq ndown 0)))
                                 (and down (org-table-maybe-recalculate-line))
                                 (or suppress-align (and org-table-may-need-update
                                                         (org-table-align))))))
                           )


(defun org-babel-execute:sql (body params)
  "Execute a block of Sql code with Babel.
This function is called by `org-babel-execute-src-block'."
  (let* ((result-params (cdr (assoc :result-params params)))
         (cmdline (cdr (assoc :cmdline params)))
         (dbhost (cdr (assoc :dbhost params)))
         (dbuser (cdr (assoc :dbuser params)))
         (dbpassword (cdr (assoc :dbpassword params)))
         (database (cdr (assoc :database params)))
         (engine (cdr (assoc :engine params)))
         (colnames-p (not (equal "no" (cdr (assoc :colnames params)))))
         (in-file (org-babel-temp-file "sql-in-"))
         (out-file (or (cdr (assoc :out-file params))
                       (org-babel-temp-file "sql-out-")))
	 (header-delim "")
         (command (case (intern engine)
                    ('dbi (format "dbish --batch %s < %s | sed '%s' > %s"
				  (or cmdline "")
				  (org-babel-process-file-name in-file)
				  "/^+/d;s/^\|//;s/(NULL)/ /g;$d"
				  (org-babel-process-file-name out-file)))
                    ('monetdb (format "mclient -f tab %s < %s > %s"
                                      (or cmdline "")
                                      (org-babel-process-file-name in-file)
                                      (org-babel-process-file-name out-file)))
                    ('msosql (format "osql %s -s \"\t\" -i %s -o %s"
                                     (or cmdline "")

                                     (org-babel-process-file-name in-file)
                                     (org-babel-process-file-name out-file)))
                    ('teradata (format "~/.emacs.d/bteq_org.sh %s %s < %s > %s"
				    (if colnames-p "" "-N")
                                    (or cmdline "")
				    (org-babel-process-file-name in-file)
				    (org-babel-process-file-name out-file)))

                    ('mysql (format "mysql %s %s %s < %s > %s"
				    (dbstring-mysql dbhost dbuser dbpassword database)
				    (if colnames-p "" "-N")
                                    (or cmdline "")
				    (org-babel-process-file-name in-file)
				    (org-babel-process-file-name out-file)))
		    ('postgresql (format
				  "psql -A -P footer=off -F \"\t\"  -f %s -o %s %s"
				  (org-babel-process-file-name in-

                                                               file)
				  (org-babel-process-file-name out-file)
				  (or cmdline "")))
                    (t (error "No support for the %s SQL engine" engine)))))
    (with-temp-file in-file
      (insert
       (case (intern engine)
	 ('dbi "/format partbox\n")
	 (t ""))
       (org-babel-expand-body:sql body params)
       (case (intern engine)
         ('teradata "\n")
         (t ""))))
    (message command)
    (org-babel-eval command "")
    (org-babel-result-cond result-params
      (with-temp-buffer
	  (progn (insert-file-contents-literally out-file) (buffer-string)))
      (with-temp-buffer
	(cond
	  ((or (eq (intern engine) 'mysql)
               (eq (intern engine) 'teradata)
	       (eq (intern engine) 'dbi)
	       (eq (intern engine) 'postgresql))
	   ;; Add header row delimiter after column-names header in first line
	   (cond
	    (colnames-p
	     (with-temp-buffer
	       (insert-file-contents out-file)
	       (goto-char (point-min))
	       (forward-line 1)
	       (insert "-\n")
	       (setq header-delim "-")
	       (write-file out-file)))))
	  (t
	   ;; Need to figure out the delimiter for the header row
	   (with-temp-buffer
	     (insert-file-contents out-file)
	     (goto-char (point-min))
	     (when (re-search-forward "^\\(-+\\)[^-]" nil t)
	       (setq header-delim (match-string-no-properties 1)))
	     (goto-char (point-max))
	     (forward-char -1)
	     (while (looking-at "\n")
	       (delete-char 1)
	       (goto-char (point-max))
	       (forward-char -1))
	     (write-file out-file))))
	(org-table-import out-file '(16))
	(org-babel-reassemble-table


	 (mapcar (lambda (x)
		   (if (string= (car x) header-delim)
		       'hline
		     x))
		 (org-table-to-lisp))
	 (org-babel-pick-name (cdr (assoc :colname-names params))
			      (cdr (assoc :colnames params)))
	 (org-babel-pick-name (cdr (assoc :rowname-names params))
			      (cdr (assoc :rownames params))))))))

          ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;;  CHEAT SHEETS
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun cheatsheet-open ()
  (interactive)
  (find-file-other-window (concat "~/.dotfiles/emacs-cheatsheets/" (format "%s" major-mode) ".org")))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;;  KEYBINDINGS
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-c c")           'compile)
(global-set-key (kbd "C-=")             'text-scale-increase)
(global-set-key (kbd "C--")             'text-scale-decrease)
(global-set-key (kbd "C-c f i")         'file-info)
(global-set-key (kbd "C-c s l")         'sql-start-local-mysql)
(global-set-key (kbd "C-c s m")         'sql-start-mysql)
(global-set-key (kbd "C-c s t")         'sql-start-teradata)
(global-set-key (kbd "C-c s l")         'teradata-send-latest-results-to-excel)
(global-set-key (kbd "C-c s r")         'teradata-send-region-to-excel)
(global-set-key (kbd "C-c s d")         'teradata-format-as-date)
(global-set-key (kbd "C-c s c")         'teradata-format-as-currency)
(global-set-key (kbd "C-c s n")         'teradata-format-as-number)
(global-set-key (kbd "C-c s p")         'teradata-format-as-percent)
(global-set-key (kbd "C-c s i")         'helm-sql-info)
(global-set-key (kbd "C-c s s")         'sql-sort-column)
(global-set-key (kbd "C-c s v")         'sql-column-calc)

(global-set-key (kbd "C-c t i")         'iawriter)
(global-set-key (kbd "C-c t w")         'wombat)
(global-set-key (kbd "C-c w f")         'flip-windows)
(global-set-key (kbd "C-c h c")         'cheatsheet-open)
(global-set-key (kbd "C-x f")           'helm-for-files)
(global-set-key (kbd "C-c h i")         'helm-imenu)
(global-set-key (kbd "C-c w r")         'rotate-windows)
(global-set-key (kbd "<C-return>")      'open-next-line)
(global-set-key (kbd "<M-return>")      'open-previous-line)
(global-set-key (kbd "M-RET")           'open-previous-line)
(global-set-key "%"		        'match-paren)               
(global-set-key [142607065]             'ns-do-hide-others)         
(global-set-key (kbd "<C-s-268632070>") 'mac-toggle-max-window)
(global-set-key (kbd "C-^")             'scroll-up-line)
(global-set-key (kbd "M-^")             'scroll-down-line)
;(windmove-default-keybindings)                

(put 'downcase-region 'disabled nil)




(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("Marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ))
(package-initialize)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;;  HELM
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'helm-config)

(setq helm-idle-delay 0.1)
(setq helm-input-idle-delay 0.1)
(setq helm-locate-command (concat "~/.dotfiles/locate-with-mdfind" " %s %s"))

(add-to-list 'load-path "~/.emacs.d/sql-info")
(require 'helm-sql-info)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;;  AG
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(defun imenu-elisp-sections ()
  (setq imenu-prev-index-position-function nil)
  (add-to-list 'imenu-generic-expression '("Sections" "^;;\\ \\ \\(.+\\)$" 1) t))

(add-hook 'emacs-lisp-mode-hook 'imenu-elisp-sections)


;; (require 'key-chord)
;; (key-chord-mode 1)
;; (key-chord-define-global ",," 'dabbrev-expand)
;; (key-chord-define-global "uu" 'undo)


(add-to-list 'load-path "~/.emacs.d/sql-info")
(require 'helm-sql-info)

(global-set-key (kbd "C-c q") 'helm-sql-info)

;; (add-hook 'dired-mode-hook (lambda() (
;; (defun dired-launch-file ()
;;   "In dired, launch the file with the associated OS X application"
;;   (interactive)
;;   (let* ((file (dired-get-filename nil t)))
;;     (message "Opening %s..." file)
;;     (call-process "open" nil 0 nil file)
;;     (message "Opening %s done" file)))



;; (define-key dired-mode-map (kbd "z") 'dired-launch-file)
;; ))

(require 'whitespace)
;(setq whitespace-style '(face lines-tail))
;(global-whitespace-mode t)


;; Go to first character indented
;; When at $ and you hit enter, it opens a line but breaks the indentation of the line below
;; Get point to stay in the same place when you do C-v and M-v
;; Jump between points

(load "~/.emacs.d/.emacs.secret")


;;  C-x r j + letter to jump to the file
(mapcar
 (lambda (r)
   (set-register (car r) (cons 'file (cdr r))))
 '((?e . "~/.emacs")
   (?l . "~/.bash_profile")))





(defun sacha/smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'sacha/smarter-move-beginning-of-line)



(server-start)



(put 'narrow-to-region 'disabled nil)
