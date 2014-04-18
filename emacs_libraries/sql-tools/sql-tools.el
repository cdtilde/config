;;; sql-tools.el --- Emacs functions for interacting with SQL buffers
;;

;;; License:
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;      http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;;; Commentary:
;; 
;; To use this file, put something like the following in your
;; ~/.emacs:
;;
;; (add-to-list 'load-path "/directory/containing/sql-tools/")
;; (require 'sql-tools)
;;


;;; Code:

(require 'sql)

(defun sql-start-session (script product)
  "Connect to a database, open a SQL window and split horizontally.
   ARG is the shell script that will be executed to create the SQL session."
  (setq mybuffername (concat "~/Documents/SQL History/"
			     (format-time-string "%Y-%m-%d - %H-%M-%S")
			     ".sql"))
  (find-file-other-window mybuffername)
  (sql-set-product product)
  (cond ((string= product "db2")
	 (setq sql-db2-program script)
	 (sql-db2))
	((string= product "sqlite")
	 (setq sql-sqlite-program script)
	 (sql-sqlite))
	((string= product "mysql")
	 (setq sql-mysql-program script)
	 (sql-mysql))))

(defun sql-start-teradata ()
    (interactive)
    (sql-start-session "~/.emacs.d/bteq" "db2"))

(defun sql-start-mysql ()
    (interactive)
    (sql-start-session "~/.emacs.d/mysql-poc" "mysql"))

(defun sql-start-local-mysql ()
    (interactive)
    (sql-start-session "~/.emacs.d/mysql-local" "mysql"))

(defun sql-start-sqlite3 ()
    (interactive)
    (sql-start-session "/usr/bin/sqlite3" "sqlite"))




(defun wrap-text (before after)
  (setq word-at-point (buffer-substring-no-properties (region-beginning) (region-end)))
  (setq wrapped-word (concat before word-at-point after))
  (delete-region (region-beginning) (region-end))
  (insert wrapped-word))

(defun teradata-format-as-date ()
  "Format the selected item as YYYY-MM-DD"
  (interactive)
  (wrap-text "cast((" " (format 'YYYY-MM-DD')) as char(10))"))

(defun teradata-format-as-currency ()
  "Format the selected item as -$1,200.99"
  (interactive)
  (wrap-text "" "(format '--$$ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99') "))

(defun teradata-format-as-number ()
  "Format the selected item as -1,200"
  (interactive)
  (wrap-text "" " (format '--ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ') "))


(defun teradata-send-latest-results-to-excel ()
 (interactive)
 (set-buffer "*SQL*")
 (goto-char (point-max))
 (forward-line -2)
 (let ((beg (point)))
  (search-backward " *** Total elapsed time was" nil)
  (forward-line 2)
  (let ((end (point)))
    (teradata-send-to-excel beg end)
)))

(defun teradata-send-region-to-excel ()
 (interactive)
    (teradata-send-to-excel (point) (mark)))

(defun teradata-send-to-excel (beg end)
" the results from *SQL* and import to Excel"
 (interactive)
 (message "Beginning = %s, end = %s" beg end)
 (set-buffer "*SQL*")
 (let ((x (make-temp-file "foo")))
   (write-region beg end x)
   (let ((command (concat "osascript -e \"set falias to (POSIX file \\\"" x "\\\")\"" 
                        " -e \"tell application \\\"Microsoft Excel\\\"\""
                        " -e \"activate\""
                        " -e \"open text file filename (falias as text) origin Macintosh start row 1 data type delimited field info {{1, text format} } other char \\\"|\\\" with use other\""
                        " -e \"end tell\""
                        )))
 (start-process-shell-command "foo-command"  "foo" command))))

;; TODO add sql-mode-hook to hook the .sql buffer up to a running *SQL* buffer, if active

(require 'sql)
(setq comint-scroll-to-bottom-on-input t)
(setq comint-scroll-to-bottom-on-output t)



;; SQL Mode Keybindings

(define-key sql-mode-map "\C-c\C-d" 'teradata-format-as-date)
(define-key sql-mode-map "\C-c\C-m" 'teradata-format-as-currency)
(define-key sql-mode-map "\C-c\C-n" 'teradata-format-as-number)
(define-key sql-mode-map "\C-c\C-e" 'teradata-send-latest-results-to-excel)
(define-key sql-mode-map "\C-c\C-g" 'teradata-send-region-to-excel)


(provide 'sql-tools)

;;; nav.el ends here

