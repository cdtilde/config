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

;; Ignore all bells!
(setq ring-bell-function 'ignore)

;; Highlight what will be replaced with M-%
(setq query-replace-highlight t)

(fset 'yes-or-no-p 'y-or-n-p)

;; When opening a file that is a symbolic link under source control,
;; don't prompt for confirmation
(setq vc-follow-symlinks t)

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

(setq mac-option-modifier 'meta) 

;; Set up the fringe, scrollbars and toolbars
(if (display-graphic-p)
    (progn
      (fringe-mode 0)
      (tool-bar-mode 0)
      (scroll-bar-mode 0)
      (menu-bar-mode 1))
  (progn
    (menu-bar-mode 0)))

;; Save minibuffer history
(savehist-mode 1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;;  SQL 
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun load-sql ()
  (interactive)
  (progn
    (add-to-list 'load-path "~/.dotfiles/emacs_libraries/sql-tools")
    (require 'sql-tools)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;;  LOOK
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun look-default()  
  (interactive)
  (progn
    (set-default-font "-*-Cousine-normal-normal-normal-*-12-*-*-*-m-0-iso10646-1")
     (add-to-list 'default-frame-alist '
                  (font . "-*-Cousine-normal-normal-normal-*-12-*-*-*-m-0-iso10646-1"))
     ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;;  WINDOWS
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
                            (setq c-basic-offset 2)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;;    MOUSE SCROLLING
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Turn on horizontal scrolling with mouse wheel
(global-set-key [wheel-right] 'scroll-left)
(global-set-key [wheel-left]  'scroll-right)

(put 'scroll-left 'disabled nil)

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;;    Keybindings
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-c s m") 'sql-start-mysql)
(global-set-key (kbd "C-c s l") 'sql-start-local-mysql)
(global-set-key (kbd "C-c s t") 'sql-start-teradata)
;;(global-set-key (kbd "C-c c")   'compile)
(global-set-key  [142607065]		'ns-do-hide-others)         ;; Command-Option-H to hide other apps
(global-set-key (kbd "<C-s-268632070>") 'mac-toggle-max-window)     ;; C-Command-F for full screen
(global-set-key (kbd "C-c w")   'flip-windows)
