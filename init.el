;; To use folding: turn on folding mode and use F7/M-F7/M-S-F7

;; Customize according to the machine
(defvar hostname (downcase (system-name))) 
(defvar emacs-profile 
  (cond ((string= hostname "leo-fujitsu-xp") 'windows-1)
	((string= hostname "matroskin") 'linux-1)
	((string= hostname "leo-gateway") 'linux-gateway)
	(t 'linux-default)))
(defvar master-session (getenv "EMACS_MASTER"))
(setenv "IN_SCREEN" "0") ;; if IN_SCREEN is set, emacs shell prompt misreads escapes intended for screen

;; Because GNOME refuses to divulge environment variables without some voodoo
;; set them up here 
(setenv "R_PATH"
	(if (eq system-type 'windows-nt)
	    (concat
	     "e:\\code\\R\\addons" ";"
	     "e:\\code\\R\\addons\\misc" ";"
	     (getenv "R_PATH"))
	   (concat
	    "/home/leo/code/R/addons/" ":"
	    "/home/leo/code/R/addons/misc/" ":"
	    (getenv "R_PATH")))) 


;;{{{ utility elisp functions

(defun lva-string-match-in-list (regex lst)
  "Returns the indices where there are regex matches in the list, similar to
the grep command in R"
  (delq nil (let ((idx -1)) 
	      (mapcar (lambda (x) (progn (setq idx (1+ idx)) (if x idx)))
		      (mapcar (lambda (x) (string-match
					   regex x)) lst)))))
(defun lva-get-first-matching-string (regex lst)
  "Return the first string in list that matches the regex"
  (let ((idx (car (lva-string-match-in-list regex lst))))
    (if idx (nth idx lst)
      nil)))


(defun lva-show-buffer-name-and-put-on-kill-ring () (interactive)
 ; (describe-variable 'buffer-file-name)
  (kill-new buffer-file-name)
  (minibuffer-message (concat "Filename [copied]:" buffer-file-name))
)
(global-set-key "\C-cn" 'lva-show-buffer-name-and-put-on-kill-ring)


;;}}}

;; bind cnotes and memos to keys:
(defvar lva-quick-file-1 "memos\\.txt\\'")
(defvar lva-quick-file-2 "cnotes\\.org\\'")
(defvar lva-quick-file-3 "imageshack\\.org\\'")



;; filter recentf-list to get full path by doing regex matching;

;; then call find-file?...


;; Increase the memory reserved
(setq gc-cons-threshold 80000000)
(setq garbage-collection-messages t) 
;; Set the load path for the default elisp directory
(setq load-path (append (list (expand-file-name "~/.emacs.d/elisp"))
			load-path))

;; start server
(if master-session (server-start))

(if master-session (desktop-save-mode 1))

;; auto-fill defaults:
(add-hook 'text-mode-hook 'turn-on-auto-fill)
;;{{{ fix auto-fill for rcirc:

;;
;; dynamically set fill-column at redisplay time
;;
(defvar dim:dynamic-fill-column-margin 3
  "Safety margin used to calculate fill-column depending on window-width")

;; dynamically set fill-column at redisplay time
(defun dim:dynamic-fill-column-window (window &optional margin)
  "Dynamically get window's width and adjust fill-column accordingly"
  (with-current-buffer (window-buffer window)
    (when (eq major-mode 'rcirc-mode)
      (setq fill-column
	    (- (window-width window) 
	       (or margin dim:dynamic-fill-column-margin))))))

(defun dim:dynamic-fill-column (frame)
  "Dynamically tune fill-column for a frame's windows at redisplay time"
  (walk-windows 'dim:dynamic-fill-column-window 'no-minibuf frame))
  
(eval-after-load 'rcirc
  '(add-to-list 'window-size-change-functions 'dim:dynamic-fill-column))

;;}}}
;; enable winner mode for swiching windows configurations
(when (fboundp 'winner-mode)
  (winner-mode 1))

;;{{{ -- anything.el and anything-config

(require 'anything)
(require 'anything-config)
(setq anything-sources
      (list anything-c-source-buffers+
	    anything-c-source-recentf
	    anything-c-source-files-in-current-dir
            anything-c-source-info-pages
            anything-c-source-file-name-history
            anything-c-source-man-pages
	    anything-c-source-file-cache
            anything-c-source-emacs-commands))
(global-set-key (kbd "\C-xc") 'anything)
(global-set-key (kbd "\C-xx") 'anything)
(global-set-key "\C-c\M-z" 'zap-to-char)
(global-set-key "\M-Z" 'zap-to-char)
(define-key anything-map "\t" 'anything-next-line)
(define-key anything-map [(control tab)] 'anything-select-action)
(define-key anything-map [(shift tab)] 'anything-previous-line)
(define-key anything-map [backtab] 'anything-previous-line)

;;}}}

;;{{{ `-- Interface / appearance settings

(when (not (eq (symbol-value 'window-system) nil))
;;  (color-theme-whatever)
  (show-paren-mode nil) ;; somehow makes parens work in terminal
  (set-frame-height (selected-frame) 37))

;; Set the buffer size for Windows 
;; good defaults for 1280x768 desktop and double-level horizontal 
;; taskbar: L 200, T 0, H 41, W 90
;; (add-to-list 'default-frame-alist '(left . 0))
;; (add-to-list 'default-frame-alist '(top . 0))
;; (add-to-list 'default-frame-alist '(height . 47))
;; (add-to-list 'default-frame-alist '(width . 90))

					;(set-default-font "Bitstream Vera Sans Mono-10")
					;(set-default-font "Consolas-11")
(if (eq emacs-profile 'linux-1)
    (set-default-font "DejaVu Sans Mono-11")
  (set-default-font "DejaVu Sans Mono-10")
  )
(setq inhibit-startup-message t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(when (not (eq (symbol-value 'window-system) nil))
  (scroll-bar-mode -1))


(setq transient-mark-mode t)
(column-number-mode 1)
(require 'paren)
(show-paren-mode 1)

;;}}}


;; re-builder extension that allows perl syntax:
;(add-to-list 'load-path (expand-file-name "~/.emacs.d/elisp"))
(require 're-builder-x)

					; fix copy/paste in Linux?..
(when (eq emacs-profile 'linux-1)
  (setq x-select-enable-clipboard t)
  (setq interprogram-paste-function 'x-cut-buffer-or-selection-value)
  )

					; Switch between windows using shift-arrows
(windmove-default-keybindings)
(global-set-key (kbd "C-S-p") 'windmove-up)
(global-set-key (kbd "C-S-n") 'windmove-down)
(global-set-key (kbd "C-<tab>") 'other-window)

;;{{{ Customize comment-style (and other newcomment.el options)

(setq comment-style 'indent)
(global-set-key (kbd "M-;") 'comment-dwim-line)
(global-set-key (kbd "C-;") 'comment-dwim-line)

;; Override set-fill-column (recentf steals default binding)
(global-set-key [(control x)(meta f)] 'set-fill-column)


(defun comment-dwim-line (&optional arg)
  "Replacement for the comment-dwim command.
    If no region is selected and current line is not blank and we are not at the end of the line,
      then comment current line.
    Replaces default behaviour of comment-dwim, when it inserts comment at the end of the line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))

;;}}}


					; Make left window key act as super
;; (setq w32-lwindow-modifier 'super)

;;{{{ Autosave tweaks
(setq auto-save-interval 120)
(setq auto-save-timeout 30) 

;; Put autosave files (ie #foo#) in one place
(defvar autosave-dir (concat "~/.emacs.d/autosave.1"))
(make-directory autosave-dir t)
(defun auto-save-file-name-p (filename) (string-match "^#.*#$" (file-name-nondirectory filename)))
(defun make-auto-save-file-name () (concat autosave-dir (if buffer-file-name (concat "#" (file-name-nondirectory buffer-file-name) "#") (expand-file-name (concat "#%" (buffer-name) "#")))))

;; Put backup files (ie foo~) in one place too. (The backup-directory-alist 
;; list contains regexp=>directory mappings; filenames matching a regexp are 
;; backed up in the corresponding directory. Emacs will mkdir it if necessary.) 
(setq backup-directory-alist '(("." . "~/.emacs.d/autosave")))
(setq version-control t)
(setq delete-old-versions t)
;;}}}

;; Misc. tweaks
(add-hook 'sql-interactive-mode-hook '(lambda () (setq comint-move-point-for-output nil))) ; don't force scroll to the bottom on output
(add-hook 'shell-mode-hook '(lambda () (setq comint-move-point-for-output nil))) ; don't force scroll to the bottom on output
(fset 'yes-or-no-p 'y-or-n-p) ; stop forcing me to spell out "yes"
;; use Unix-style line endings
(setq-default buffer-file-coding-system 'undecided-unix)
;; make woman not pop up a new frame
(setq woman-use-own-frame nil)
(setq vc-follow-symlinks t)  ;; prevent version control from asking whether to follow links
(setq isearch-allow-scroll t) ;; allows minimal scrolling, as long as curr. match is visible

;;{{{ cua mode (used for its rectangle prowess)
(add-hook 'cua-mode-hook
          '(lambda () ;; don't want default C-RET behavior
             (define-key cua--rectangle-keymap [(control return)] nil)
             (define-key cua--region-keymap    [(control return)] nil)
             (define-key cua-global-keymap     [(control return)] nil)))
(cua-mode 'emacs)
(defun my-cua-rect-set-mark (&optional arg) 
  (interactive "P")
  (if (or (not mark-active) arg)
      (cua-set-mark arg)
    (cua-set-rectangle-mark)))
(global-set-key (kbd "C-@") 'my-cua-rect-set-mark);; hit C-SPC twice for the awesome rectangle editing power 
(global-set-key (kbd "C-SPC") 'my-cua-rect-set-mark);; hit C-SPC twice for the awesome rectangle editing power 
;; make C-SPC cycle mark->cua rect->unset mark
(defadvice cua--init-rectangles (after cua-rect-toggle-mark () activate)
    (define-key cua--rectangle-keymap [remap my-cua-rect-set-mark] 'cua-clear-rectangle-mark))
;; by default, cua-rect includes current cursor position into the rectangle (not how default rectangles work)
(defadvice cua-set-rectangle-mark (after cua-adjust-rect-size () activate)
    (call-interactively 'cua-resize-rectangle-left))
;;}}}

;; Default browser: Emacs doesn't seem to respect the OS defaults (prefers chromium)
;;(setq browse-url-browser-function 'browse-url-firefox)

;; Misc. keybindings
(global-set-key "\C-c\S-i" 'indent-region)
(global-set-key "\C-ci" 'imenu)
(global-set-key "\C-co" 'occur)
(global-set-key [(control c) (control o)] 'occur)
(global-set-key [(control f3)] 'explorer-here)
(global-set-key [(control super f3)] 'explorer-here)
(global-set-key [(control f4)] 'terminal-here)
(global-set-key [(control super f4)] 'terminal-here)
; alias for toggle-input-method s.t. AUCTeX electric macro could be bound to C-\
(global-set-key [(control c) (control \\)] 'toggle-input-method)
(global-unset-key [\C-down-mouse-3])
(define-key function-key-map [\C-mouse-3] [mouse-2])
; keybindings for screen running inside shell, as per
; http://blog.nguyenvq.com/2010/07/11/using-r-ess-remote-with-screen-in-emacs/
;; used to send screen keybindings to shell in emacs
(define-key shell-mode-map (kbd "C-l") (lambda (seq) (interactive "k") (process-send-string nil seq)))
(define-key inferior-ess-mode-map (kbd "C-l") (lambda (seq) (interactive "k") (process-send-string nil seq)))

; work-around for C-M-p broken in my windows
(global-set-key [(control meta shift z)] 'backward-list)
; alternative bindings for M-x as per Steve Yegge's suggestion
(defalias 'evabuf 'eval-buffer)
(defalias 'eregion 'eval-region)


;  -- faster point movement:
;; (global-set-key "\M-\C-p" 
;;   '(lambda () (interactive) (previous-line 5)))
;; (global-set-key "\M-\C-n" 
;;   '(lambda () (interactive) (next-line 5)))



;; Navigation: (todo -- unify navi-related fns)
(require 'goto-last-change)
(global-set-key "\C-x\C-\\" 'goto-last-change)
(global-set-key "\C-x\\" 'goto-last-change)
(global-set-key "\C-x|" 'goto-last-change)

;; similar effect is obtained by exchange point and mark (turn off the highlighting)
(defun transient-exchange-point-and-mark () (interactive) (exchange-point-and-mark 1))
(global-set-key "\C-x\C-x" 'transient-exchange-point-and-mark)



;; ========== Line by line scrolling ==========

;; This makes the buffer scroll by only a single line when the up or
;; down cursor keys push the cursor (tool-bar-mode) outside the
;; buffer. The standard emacs behaviour is to reposition the cursor in
;; the center of the screen, but this can make the scrolling confusing
;(setq scroll-step 1)
;; this seemed to sucks; let's try this smooth-scrolling package
;(setq scroll-step 1)
(require 'smooth-scrolling)
;; to change where the scrolling starts, customize-variable smooth-scroll-margin

;; Color-theme:
(setq load-path (append (list (expand-file-name "~/.emacs.d/elisp/color-theme-6.6.0")) load-path))
(require 'color-theme)
(when (not (eq (symbol-value 'window-system) nil)) ;(not nil)
  (color-theme-initialize)
  (color-theme-tango-2))
  ;; (color-theme-midnight))

;; thing at point mark:
(require 'thing-cmds)
(global-set-key [?\C-\M- ] 'cycle-thing-region)
(global-set-key [(meta ?@)] 'mark-thing)

;; Hippie-expand:
(global-set-key (kbd "M-/") 'hippie-expand)
(setq hippie-expand-try-functions-list '(try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill try-complete-file-name-partially try-complete-file-name try-expand-all-abbrevs try-expand-list try-expand-line try-complete-lisp-symbol-partially try-complete-lisp-symbol))

;; autopair
(require 'autopair)
(autopair-global-mode) ;; to enable in all buffers
(setq autopair-autowrap t)
;; stuff to deal with foo()bar-type situations
(fset 'autopair-paren-fwd-1
   [?\C-  right ?\C-w ?\C-\M-f ?\C-\M-f ?\C-y ?\C-\M-b ?\M-f])
(global-set-key (kbd "\C-cf") 'autopair-paren-fwd-1)
(defun autopair-skip-dollar-action (action pair pos-before)
  "Let |.| define the position of the cursor.  Want the following behavior
when pressing $: 
   $|$| -> $$|$|$, but $a|$| -> $a$| |"
  (if (and (looking-at "\\$")
	   (save-excursion
	     (backward-char)
	     (not (looking-at "\\$"))))
      (autopair-default-handle-action 'skip-quote pair pos-before)
    (autopair-default-handle-action action pair pos-before)))

(add-hook 'TeX-mode-hook
          #'(lambda ()
              (setq autopair-handle-action-fns
                    (list #'autopair-skip-dollar-action))))

;; Grep enhancements:
(add-to-list 'load-path "~/.emacs.d/elisp/grep-a-lot.git")
(require 'grep-a-lot)
(grep-a-lot-setup-keys)


;;{{{ Modify open line behavior to be like in VI (C-o open line, M-o open prev line)

;; Behave like vi's o command
(defun open-next-line (arg)
  "Move to the next line and then opens a line.
    See also `newline-and-indent'."
  (interactive "p")
  (end-of-line)
  (open-line arg)
  (next-line 1)
  (when newline-and-indent
    (indent-according-to-mode)))

(global-set-key (kbd "C-o") 'open-next-line)

;; Behave like vi's O command
(defun open-previous-line (arg)
  "Open a new line before the current one. 
     See also `newline-and-indent'."
  (interactive "p")
  (beginning-of-line)
  (open-line arg)
  (when newline-and-indent
    (indent-according-to-mode)))

(global-set-key (kbd "M-o") 'open-previous-line)

;; Autoindent open-*-lines
(defvar newline-and-indent t
  "Modify the behavior of the open-*-line functions to cause them to autoindent.")

;;}}}

;;{{{ select quotes/extend selection/do stuff with region (M-S-8,M-8,M-S-7)

(defun select-text-in-quote-balanced-base ()
"Select text between the nearest left and right delimiters.
Delimiters are paired characters: ()[]$$<>«»“”‘’「」, including \"\"."
 (interactive)
 (let (b1 b2 ldelim rdelim delim-pairs rdpos ldstring)
   (setq delim-pairs "<>()“”{}[]$$「」«»\"\"''‘’`\"")
   (skip-chars-backward "^<(“{[$「«\"'‘`")
   (setq b1 (point))
   (setq ldelim (char-before))
   (setq ldstring (make-string 1 ldelim))
   (if (or (string= ldstring "[") (string= ldstring "$")) (setq ldstring (concat "\\" ldstring)))
   (setq rdpos (1+ (string-match ldstring delim-pairs)))
   (setq rdelim (substring delim-pairs rdpos (1+ rdpos)))
;   (message "rdelim is %s." rdelim)
   (skip-chars-forward (concat "^" rdelim))
   (setq b2 (point))
   (set-mark b1)
   ))

(defun adjacent-to-matched-delims-p (start end)
  "if start and end are near matched delims, mark region including delims"
  (let (ch1 ch2)
    (when (and (char-after end) (char-before start))
      (setq ch2 (char-to-string (char-after end)))
      (setq ch1 (char-to-string (char-before start)))
      (matched-delims-p ch1 ch2))))

(defun select-text-in-quote-balanced ()
  "Select text between the nearest left and right delimiters.
   Delimiters are paired characters: ()[]$$<>«»“”‘’「」, including \"\"."
  (interactive)
   (if (and transient-mark-mode mark-active 
	    (adjacent-to-matched-delims-p (region-beginning) (region-end)))
       (progn
	 (goto-char (1+ (region-end)))
	 (set-mark (- (region-beginning) 1)))
     (select-text-in-quote-balanced-base)
     ))

(global-set-key (kbd "M-*") 'select-text-in-quote-balanced)


(defun select-text-in-quote ()
"Select text between the nearest left and right delimiters.
Delimiters are paired characters: ()[]<>«»“”‘’「」, including \"\"."
 (interactive)
 (let (b1 b2)
   (skip-chars-backward "^<>(“{[「«\"'‘")
   (setq b1 (point))
   (skip-chars-forward "^<>)”}]」»\"'’")
   (setq b2 (point))
   (set-mark b1)
   )
 )

;; by Nikolaj Schumacher, 2008-10-20. Released under GPL.
(defun semnav-up (arg)
  (interactive "p")
  (when (nth 3 (syntax-ppss))
    (if (> arg 0)
        (progn
          (skip-syntax-forward "^\"")
          (goto-char (1+ (point)))
          (decf arg))
      (skip-syntax-backward "^\"")
      (goto-char (1- (point)))
      (incf arg)))
  (up-list arg))

;; by Nikolaj Schumacher, 2008-10-20. Released under GPL.
(defun extend-selection (arg &optional incremental)
  "Select the current word.
Subsequent calls expands the selection to larger semantic unit."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     (or (and transient-mark-mode mark-active)
                         (eq last-command this-command))))
  (if incremental
      (progn
        (semnav-up (- arg))
        (forward-sexp)
        (mark-sexp -1))
    (if (> arg 1)
        (extend-selection (1- arg) t)
      (if (looking-at "\\=\\(\\s_\\|\\sw\\)*\\_>")
          (goto-char (match-end 0))
        (unless (memq (char-before) '(?\) ?\"))
          (forward-sexp)))
      (mark-sexp -1))))

(global-set-key (kbd "M-8") 'extend-selection)

(defun matched-delims-p (chstr1 chstr2)
  "Returns t if the two arguments are 1-char strings corr to ordered matched delimiters."
;  (interactive)
  (let (delim-pairs ldelim)
    (setq delim-pairs "<>()“”{}[]$$「」«»\"\"''‘’`\"")
    (if (string= chstr1 "[") (setq chstr1 (concat "\\" chstr1)))
    (setq ldelim (string-match chstr1 delim-pairs))
    (if ldelim
	(string= chstr2 (substring delim-pairs (1+ ldelim) (+ 2 ldelim)))
      nil)))


(defun add-before-after-region (start end)
  "Surrounds region with things. If {}, \"\", etc is given as
'before' string, it will surround the region with delims w/o prompting for 'after' string.  The special (**) 'before' string will surround the regin with (* and *) -- comments in OCaml and Mathematica." 
  (interactive "r")
  (let (before after pos2) 
    (setq before (read-from-minibuffer "'Before' string:"))
    (unless (or (and (eq (length before) 2)
	     (let ((ch1 (substring before 0 1)) (ch2 (substring before 1 2)))
	       (if (matched-delims-p ch1 ch2)
		   (progn
		     (setq before ch1)
		     (setq after ch2)
		     t)))) ;; dealt with matched delimiters
	     (cond ((string= before "(**)") ;hack for ocaml and mathematica
		    (progn
		      (setq before "(*")
		      (setq after "*)")
		      t))
		   ((string= before ":DE") ;for org-mode drawers
		    (progn
		      (setq before ":DETAILS:\n")
		      (setq after "\n:END:")
		      t))
		   ((or (string= before ":CODE") (string= before ":CO"))  ;for org-mode drawers
		    (progn
		      (setq before ":CODE:\n")
		      (setq after "\n:END:")
		      t)))) ;; end checking for special cases
	(setq after (read-from-minibuffer "'After' string:")))
    (setq pos2 (+ end (length before)))
    (goto-char (region-beginning)) (insert before)
    (goto-char pos2) (insert after)
    )
  )

(global-set-key (kbd "M-&") 'add-before-after-region)

;;}}}

;; highlight symbol
;; (add-to-list 'load-path "~/.emacs.d/elisp/highlight-symbol")
(require 'highlight-symbol)
(global-set-key [(meta f3)] 'highlight-symbol-at-point)

;; From Xah Lee's page:
;; temporarily set fill-column to a huge number (point-max); 
;; thus, effectively, replaces all new line chars by spaces in
;; current paragraph.
(defun remove-line-breaks () 
  "Remove line endings in a paragraph."
  (interactive) 
  (let ((fill-column (point-max))) 
    (fill-paragraph nil)))

;; (require 'chop)
;; (global-set-key "\M-p" 'chop-move-up)
;; (global-set-key "\M-n" 'chop-move-down)

;;------ Folding keys: C-c-TAB (indent acc to mode), F7/M-F7/S-M-F7 fold dwim
;;{{{ -- Folding stuff: modes, DWIM keybindings, indent-or-toggle-fold, etc

;; folding mode 
(require 'folding)
(autoload 'folding-mode          "folding" "Folding mode" t)
(autoload 'turn-off-folding-mode "folding" "Folding mode" t)
(autoload 'turn-on-folding-mode  "folding" "Folding mode" t)
(folding-add-to-marks-list 'matlab-mode "% {{{" "% }}}" nil t)
;; (folding-add-to-marks-list 'matlab-mode "%{{{" "%}}}" nil t)
(folding-add-to-marks-list 'mma-mode "(* {{{" "(* }}}" nil t)
;; (folding-add-to-marks-list 'mma-mode "(*{{{" "(*}}}" nil t)
(folding-add-to-marks-list 'mathematica-mode "(* {{{" "(* }}}" nil t)
;; (folding-add-to-marks-list 'mathematica-mode "(*{{{" "(*}}}" nil t)
(folding-add-to-marks-list 'ess-mode "## {{{" "## }}}" " ")
(folding-add-to-marks-list 'ess-mode "##{{{" "##}}}" " ")
(folding-add-to-marks-list 'ess-mode "### {{{" "### }}}" " ")
(folding-add-to-marks-list 'ahk-mode ";; {{{" ";; }}}" " ")
(folding-add-to-marks-list 'ahk-mode "; {{{" "; }}}" " ")
(if (load "folding" 'nomessage 'noerror) 
             (folding-mode-add-find-file-hook))



(require 'fold-dwim)
(global-set-key (kbd "<f7>")      'fold-dwim-toggle)
(global-set-key (kbd "<M-f7>")    'fold-dwim-hide-all)
(global-set-key (kbd "<S-M-f7>")  'fold-dwim-show-all)

;; useful to check: (check-folding-line (thing-at-point 'line))
(defun check-folding-line (line)
  "Checks if there's an evidence that this line is a start of folded
block -- if there are folding markups or if it matches outline regex"
  (or (and (string-match "{{{\\|}}}" line) (symbol-value folding-mode))
      (and (symbol-value outline-minor-mode) (string-match outline-regexp line))
      (and (symbol-value hs-minor-mode) (string-match hs-block-start-regexp line))))

(defun indent-or-toggle-fold () ; doesn't work well w/ python?
  (interactive)
  (if (minibufferp)
      (ido-next-match)
    (let ((start-point (point)))
      (indent-according-to-mode)
      (if (and (eq start-point (point)) 
	       (check-folding-line (thing-at-point 'line)))
	  (fold-dwim-toggle)))))


(defun toggle-fold-or-indent () ;backward char fails after ellipsis...
  (interactive)
  (if (minibufferp)
      (ido-next-match)
    (if (check-folding-line (thing-at-point 'line))
	(progn (skip-chars-backward "^\n") (fold-dwim-toggle))
      (indent-according-to-mode))))
    

(add-hook 'folding-mode-hook
	  '(lambda ()
	     (define-key folding-mode-map (kbd "TAB") 'toggle-fold-or-indent)
	     (define-key folding-mode-map [(tab)]'toggle-fold-or-indent))) ;'indent-or-toggle-fold)))
(add-hook 'outline-minor-mode-hook 	
	  '(lambda ()
	     (define-key outline-minor-mode-map (kbd "TAB") 'toggle-fold-or-indent)
	     (define-key outline-minor-mode-map [(tab)]
	       'toggle-fold-or-indent))) ;'indent-or-toggle-fold)))


(defadvice hs-org/hideshow (around hs-org-check-line activate)
    (if (check-folding-line (thing-at-point 'line)) 
	(save-excursion
	  (end-of-line)
	  ad-do-it)
      (indent-according-to-mode)))


(global-set-key [(control c) tab]  'indent-according-to-mode)

;; HideShow stuff:
(require 'hideshow-org)
(when window-system ;; hideshowvis crashes in terminal
  (require 'hideshowvis)
  (autoload 'hideshowvis-enable "hideshowvis" "Highlight foldable regions")
  (load-library "hideshowvis-settings"))
(add-to-list 'hs-special-modes-alist '(ess-mode "{" "}" "#" nil nil))
(add-hook 'hs-minor-mode-hook 'hs-org/minor-mode)

(add-hook 'c-mode-hook 'hs-minor-mode)
(add-hook 'c++-mode-hook 'hs-minor-mode)
(add-hook 'perl-mode-hook 'hs-minor-mode)
(add-hook 'ess-mode-hook 'hs-minor-mode)

;; (global-unset-key [f1])
;; (global-set-key [f1] 'hs-toggle-hiding)
;;~end folding stuff 

;;}}}


;; speedbar
(require 'sr-speedbar)
(global-set-key (kbd "C-S-s") 'sr-speedbar-toggle)

;; docview
;; (load-file (expand-file-name "~/.emacs/doc-view.el"))
;; ("\\.pdf$" . open-in-doc-view)
;; ("\\.dvi$" . open-in-doc-view)
;; ("\\.ps$" . open-in-doc-view)
;; (defun open-in-doc-view ()
;;   (interactive)
;;   (doc-view
;;    (buffer-file-name (current-buffer))
;;    (buffer-file-name (current-buffer))))
;; (add-hook 'doc-view-mode-hook 'auto-revert-mode)

;;{{{ -- Org-mode:

(setq load-path (cons "~/.emacs.d/elisp/org-mode.git/lisp" load-path))
;(setq load-path (cons "~/.emacs.d/elisp/org-mode.git/contrib/lisp" load-path))
(require 'org-install)
(setq org-startup-indented t)
;; The following lines are always needed.  Choose your own keys.
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cq" 'org-iswitchb)
(setq org-todo-keywords
       '((sequence "TODO" "WAIT" "|" "DONE" "CANCELED")))
(add-hook 'org-mode-hook 
	  '(lambda () (auto-fill-mode t) (setq comment-start nil)))
(setq org-return-follows-link t)
(global-font-lock-mode 1)			  ; for all buffers
(add-hook 'org-mode-hook 'turn-on-font-lock)	  ; Org buffers only
(setq org-file-apps (quote ((auto-mode . emacs) ("\\.x?html?\\'" . default)  ("\\.nb\\'" . "mathematica %s"))))
(if (eq system-type 'windows-nt)
  (setq org-file-apps (cons '("\\.pdf\\'" . "C:\\Program Files\\Adobe\\Acrobat 8.0\\Acrobat\\Acrobat.exe %s") org-file-apps))
  (setq org-file-apps (cons '(" \\.pdf::\\([0-9]+\\)\\'" . "evince %s -p %1") org-file-apps))
  (setq org-file-apps (cons '("\\.pdf\\'" . "evince %s") org-file-apps)))

;; active Babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t) (sh . t) (python . t) (perl . t) (matlab . t) (latex . t)))

;; Make windmove work in org-mode:
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

;;}}}

;; fix misbehaving overloaded temp-buffer display function
(defadvice org-goto (around dont-focus-temp-buffer activate)
  (let ((temp-buffer-show-function nil)) ad-do-it))
;; override default list-buffers to use pop-to-buffer
(defadvice list-buffers (around pop-to-list-buffers activate)
    (pop-to-buffer (list-buffers-noselect files-only)))


;;{{{ -- Windows/cygwin-related settings 

(when (eq system-type 'windows-nt)
  (require 'cygwin-mount)
  (cygwin-mount-activate)
  (require 'w32-symlinks)
 ;(require 'setup-cygwin)
  )


;; Make dired sort case-insensitive on Windows
(when (eq system-type 'windows-nt)
  (setq ls-lisp-emulation   'MS-Windows
	ls-lisp-dirs-first  t
	ls-lisp-ignore-case t
	ls-lisp-verbosity   (nconc (and (w32-using-nt)
					'(links)) '(uid))))


;; Replace DOS shell w/ bash ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(let ((var (getenv "HOME")))
;  home-env-var
;  )

;(setenv "HOME" "C:\\cygwin\\home\\Leo")

(when (eq system-type 'windows-nt)
  (add-hook 'comint-output-filter-functions
	    'shell-strip-ctrl-m nil t)
  (add-hook 'comint-output-filter-functions
	    'comint-watch-for-password-prompt nil t)
  (setq exec-path (cons "C:/cygwin/bin" exec-path))
  (setenv "PATH" (concat "C:\\cygwin\\bin;" (getenv "PATH")))
  (setq explicit-shell-file-name "bash")
  (setq explicit-bash-args '("--login" "-i"))
  ;;C:\cygwin\bin\bash -c "/bin/xhere /bin/bash.exe '%L'"
  ;; For subprocesses invoked via the shell
  ;; (e.g., "shell -c command")
  (setq shell-file-name explicit-shell-file-name)
)

;;}}}


;;{{{ -- Explorer here / terminal here functions (Windows)

; Windows explorer to go to the file in the current buffer
;; (defun explorer-here ()  
;;   "Call when editing a file in a buffer. Open windows explorer in the current directory and select the current file"  
;;   (interactive)  
;;   (w32-shell-execute 
;;     "open" "explorer"  
;;     (concat "/e,/select," (convert-standard-filename buffer-file-name))
;;   )
;; )

(defun explorer-here ()   
  "Open Windows Explorer to FILE (a file or a folder)."
  (interactive)
  (let ((w32file "") (dir ""))
    (if
	(and (local-variable-p 'dired-directory) dired-directory)
	(setq w32file (substitute ?\\ ?/ (expand-file-name (convert-standard-filename dired-directory))))
      (setq w32file (substitute ?\\ ?/ (expand-file-name (convert-standard-filename buffer-file-name))))
      )
    (if (file-directory-p w32file)
	(w32-shell-execute "explore" w32file "/e,/select,")
      (w32-shell-execute "open" "explorer" (concat "/e,/select," w32file)))))

(defun terminal-here ()   
  "Launch external terminal in the current buffer's directory or current dired
directory.  (Works by grabbing the directory name and passing as an argument to
a batch file.  Note the (toggle-read-only) workaround; the command will not run
in dired mode without it."
  (interactive)
  (let ((dir "") (diredp nil))
    (cond
     ((and (local-variable-p 'dired-directory) dired-directory)
      (setq dir dired-directory)
      (setq diredp t)
      (toggle-read-only)
)
     ((stringp (buffer-file-name))
      (setq dir (file-name-directory (buffer-file-name))))
      )
    (shell-command (concat "~/bin/mrxvt_win.bat \""dir"\" 2>/dev/null &") 
 (universal-argument))
    (if diredp (toggle-read-only))
))


;; try external ls as per ntemacs faq 7.9 (doesn't work well)
;; (when (eq system-type 'windows-nt)
;;   (setq ls-lisp-use-insert-directory-program t)      ;; use external ls
;;   (setq insert-directory-program "c:/cygwin/bin/ls") ;; ls program name
;; )

;;}}}

;; swap / transpose windows (steve yegge)
(defun swap-windows ()
 "If you have 2 windows, it swaps them." (interactive) (cond ((not (= (count-windows) 2)) (message "You need exactly 2 windows to do this."))
 (t
 (let* ((w1 (first (window-list)))
	 (w2 (second (window-list)))
	 (b1 (window-buffer w1))
	 (b2 (window-buffer w2))
	 (s1 (window-start w1))
	 (s2 (window-start w2)))
 (set-window-buffer w1 b2)
 (set-window-buffer w2 b1)
 (set-window-start w1 s2)
 (set-window-start w2 s1)))))
(define-key ctl-x-4-map (kbd "t") 'swap-windows)

;; TODO:
;; what needs to happen re: kill-window-other-buffer:
;; need to check the winner stack and see if the last change was a window config
;; change or just a buffer change; if config change then winner-undo o/w just kill
(defun kill-buffer-other-window (arg)
  "Kill the buffer in the other window,
 and make the current buffer full size.
 If no other window, kills current buffer."
  (interactive "p")
  (let ((buf (save-window-excursion (other-window arg) (current-buffer))))
    (delete-windows-on buf) (kill-buffer buf)) (winner-undo))
(define-key ctl-x-4-map (kbd "k") 'kill-buffer-other-window)


;; (defvar my-display-buffer-list)
;; (add-to-list 'my-display-buffer-list "*TeX Help*")

;; ;b (setq display-buffer-function (quote my-display-buffer))


;; (defun my-display-buffer (buffer-or-name &optional not-this-window frame)
;;   (let (display-buffer-function window)
;;     (setq window (display-buffer buffer-or-name not-this-window))
;;     (when (member (buffer-name buffer-or-name) my-display-buffer-list)
;;       ;(debug)
;;       (select-window window)
;;       ;(view-mode t)
;;       (message "FOOBAR")
;;       )
;;     (set-window-buffer window buffer)
;;     (select-window window)
;;     window))



;; ispell:
(when (eq system-type 'windows-nt)
    (setq ispell-program-name "C:/Program Files/Aspell/bin/aspell.exe")
)

;; git
(if (eq system-type 'windows-nt)
    (require 'git-mswin)
  (require 'git))
(setq load-path (cons "~/.emacs.d/elisp/magit" load-path))
(require 'magit)

;;{{{ dired enhancements:

(require 'dired-details)
;; (dired-details-install)  ;;;; this seems to break dired, TODO: fix
(require 'dired+)
(toggle-dired-find-file-reuse-dir 1)	; show subdirs in same buffer

(define-key dired-mode-map [(backspace)] 'dired-up-directory) 
(define-key dired-mode-map (kbd "DEL") 'dired-up-directory)  ; need when working
					; in terminal


(defun dired-show-only (regexp)		; show only files that match a 
   (interactive "sFiles to show (regexp): ") ; regex (e.g. .*nb$ to only
   (dired-mark-files-regexp regexp)	     ; show .nb files)
   (dired-toggle-marks)
   (dired-do-kill-lines))
(define-key dired-mode-map [?%?h] 'dired-show-only) 

;; rename the dired buffer; take care of possible buffer name collisions
(defun buffer-exists (bufname) (not (eq nil (get-buffer bufname)))) 
(add-hook 'dired-after-readin-hook	; put "dired" in buffer name
          #'(lambda () (unless (string-match "*dired*" (buffer-name))
			 (let ((new-buf-name (concat "*dired* "
						     (buffer-name))) (count 1))
			   (while (buffer-exists new-buf-name)
			     (setq new-buf-name (concat new-buf-name "|"
							(number-to-string
							 count)))
			     (setq count (1+ count)))
			   (rename-buffer new-buf-name)))))

;;    (add-hook 'dired-load-hook
;;             (lambda () (load "dired-x") 
;;                  ;; set dired-x global variables here.))
;; ^^^ commented out b/c dired-x is loaded by dired+
(add-hook 'dired-mode-hook		; requires dired-x 
	  (function (lambda () ; Set dired-x buffer-local variables here. 
		      ;; (dired-omit-mode 1) ; turn on omit mode and
 		      ;; (setq dired-omit-files ; omit the "dot" files
		      ;; 	    (concat dired-omit-files "\\|^\\..+$"))
		      )))


;; unfill paragraph (remove hard linebreaks; use w/ longlines mode)
;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
;; Takes a multi-line paragraph and makes it into a single line of text.
(defun unfill-paragraph ()
  (interactive)
  (let ((fill-column (point-max)))
  (fill-paragraph nil)))

(defun unfill-region (start end)
  (interactive "r")
  (let ((fill-column (point-max)))
    (fill-region start end nil)))

;;}}}

;;{{{ search enhancements:

;; Use isearch+ (cf http://www.emacswiki.org/emacs/IsearchPlus)
(eval-after-load "isearch" '(require 'isearch+))
   ; avoid automatic mark that persists when terminating search w/ arrow keys:
(eval-after-load "isearch+" '(setq isearchp-set-region-flag nil)) 
(define-key isearch-mode-map [(control ? )] 'set-region-around-search-target)
; control-SPC, overrides the default isearchp setting of isearchp-toggle-set-region
(global-set-key [f11] 'isearch-forward)
(define-key isearch-mode-map [f11] 'isearch-repeat-forward)
(global-set-key [(shift f11)] 'isearch-backward)
(define-key isearch-mode-map [(shift f11)] 'isearch-repeat-backward)
;; (define-key view-mode-map (kbd "/") 'isearch-forward)
(define-key dired-mode-map (kbd "/") 'isearch-forward)
; overrides default mark directories
(define-key isearch-mode-map (kbd "<C-n>") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "<C-/>") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "<C-p>") 'isearch-repeat-backward)
(define-key isearch-mode-map (kbd "<C-?>") 'isearch-repeat-backward)

;; modify C-s C-w to match whole word
(defun my-isearch-yank-word-or-char-from-beginning ()
  "Move to beginning of word before yanking word in isearch-mode."
  (interactive)
  ;; Making this work after a search string is entered by user
  ;; is too hard to do, so work only when search string is empty.
  (if (= 0 (length isearch-string))
      (beginning-of-thing 'word))
  (isearch-yank-word-or-char)
  ;; Revert to 'isearch-yank-word-or-char for subsequent calls
  (substitute-key-definition 'my-isearch-yank-word-or-char-from-beginning 
			     'isearch-yank-word-or-char
			     isearch-mode-map))

(add-hook 'isearch-mode-hook
 (lambda ()
   "Activate my customized Isearch word yank command."
   (substitute-key-definition 'isearch-yank-word-or-char 
			      'my-isearch-yank-word-or-char-from-beginning
			      isearch-mode-map)))

;;}}}

;; turn on view mode for read-only files
(setq view-read-only t)

;(require 'viewer)
;(viewer-stay-in-setup)
;(setq view-mode-by-default-regexp "/regexp-to-path")
;; (defun temp-mode-view (buffer)
;;   (message "TEEEEEEEEEEEEEEMP")
;; )
;; ;; (if (string-match (buffer-name buffer) "*Help*")
;; ;;       (view-mode buffer)))

;; (setq temp-buffer-show-hook 'temp-mode-view) 

;; (global-set-key (kbd "C-/") 'isearch-forward)  ; conflicts w/ undo?


;; w3m
(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
(setq w3m-use-cookies t)
(autoload 'w3m-type-ahead-mode "wta" t)
(add-hook 'w3m-mode-hook 'w3m-type-ahead-mode)

;; mew
;; (add-to-list 'load-path "~/.emacs.d/elisp/mew-6.2.51")
;; (autoload 'mew "mew" nil t)
;; (autoload 'mew-send "mew" nil t)



;; tramp
(require 'tramp)
(setq tramp-verbose 2)
(setq tramp-default-method "ssh")
(setq tramp-debug-buffer nil)
(setq tramp-password-end-of-line "\r\n")

(defun sudo-edit (&optional arg)
  (interactive "p")
  (if arg
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))
 
(defun sudo-edit-current-file ()
  (interactive)
  (let ((pos (point)))
    (find-alternate-file (concat "/sudo:root@localhost:" (buffer-file-name (current-buffer))))
    (goto-char pos)))

(when (eq system-type 'windows-nt)
  (nconc (cadr (assq 'tramp-login-args (assoc "ssh" tramp-methods)))
	 '(("bash" "-i")))
  (setcdr (assq 'tramp-remote-sh (assoc "ssh" tramp-methods))
	  '("bash -i")))


;; stuff that was necessary to get tramp to work under cygwin
; (setq load-path (append load-path '("/usr/share/emacs/22.1/lisp/emacs-lisp")))
; (setq load-path (append load-path '("/usr/share/emacs/22.1/lisp/calendar")))
; (require 'tramp)



;;{{{ Language modes: scheme/ahk/mathematica/matlab

;; needed just for Matlab(?)  Part of ECB:
;; (load-file (expand-file-name 
;; 	    "~/.emacs.d/elisp/cedet-1.0pre7/common/cedet.el"))
;; (semantic-load-enable-minimum-features)
;; (semantic-load-enable-code-helpers) 
;; (if window-system ;; don't turn this on in terminal mode
;;     (global-semantic-tag-folding-mode))
;; (add-to-list 'load-path "~/.emacs.d/elisp/ecb")
;; (require 'ecb-autoloads)

;; Python:
(defun my-python-eval ()
  "python-shell-run-region-or-defun-and-go"
   (interactive)
 (if (and transient-mark-mode mark-active)
     (python-send-region (mark) (point))
   (python-send-defun))
 (deactivate-mark)
 (python-switch-to-python t))

(defun my-python-mode-hook ()
  (define-key python-mode-map (kbd "C-c r") 'my-python-eval)
  (define-key python-mode-map (kbd "C-c RET") 'my-python-eval)
  (define-key python-mode-map [f1] 'my-python-documentation)
  (define-key inferior-python-mode-map [f1] 'my-python-documentation)
  ;; (local-set-key (kbd "C-c RET") 'my-python-eval)
  (define-key python-mode-map [(shift return)] 'my-python-eval))
(add-hook 'python-mode-hook 'my-python-mode-hook) 

(add-hook 'inferior-python-mode-hook
	  '(lambda()
	     (local-set-key "\M-o" 'prev-input-goto-paren)))

(defun my-python-documentation (w)
  "Launch PyDOC on the Word at Point"
  (interactive
   (list (let* ((word (thing-at-point 'word))
		(input (read-string 
			(format "pydoc entry%s: " 
				(if (not word) "" (format " (default %s)" word))))))
	   (if (string= input "") 
	       (if (not word) (error "No pydoc args given")
		 word) ;sinon word
	     input)))) ;sinon input
  (shell-command (concat python-command " -c \"from pydoc import help;help(\'" w "\')\"") "*PYDOCS*")
  (view-buffer-other-window "*PYDOCS*" t '(lambda (arg) (quit-window t))))


;; Scheme:
(when (eq emacs-profile 'windows-1)
  (setq scheme-program-name "C:/Program-Files/MzScheme/mzscheme")
) ; had to create junction via junction c:\Program-Files "c:\Program Files"
(require 'quack)

(setq auto-mode-alist
        (cons '("\\.sc$" . scheme-mode)
                auto-mode-alist))

;; Haskell:
(add-to-list 'load-path "~/.emacs.d/elisp/haskell-mode-2.7.0")
(load "haskell-site-file")

;; Autohotkey (Windows-specific)
;; choose ahk-mode rather than ahk-org mode:
(when (eq emacs-profile 'windows-1)
  (setq ahk-syntax-directory "C:/Program Files/AutoHotkey/Extras/Editors/Syntax/")
  (add-to-list 'auto-mode-alist '("\\.ahk$" . ahk-mode))
  (autoload 'ahk-mode "ahk-mode"))


;; mathematica mode -- there are two files: mathematica.el and mma.el
;; one provides support for interactive evaluation (mathematica), the other provides
;; better dev facilities (imenu support, etc) (mma)
(load-file "~/.emacs.d/elisp/mathematica.el")
;; (setq auto-mode-alist (append '(("\\.mma\\'" . mathematica-mode))
(load-file "~/.emacs.d/elisp/mma.el")
(setq auto-mode-alist (append '(("\\.mma\\'" . mma-mode))
			      auto-mode-alist))
(setq mathematica-never-start-kernel-with-mode t)
(setq mma-outline-regexp "^\\w+\\[.*\\][^;\n]*:=\\|^\\w+::usage\\|^\\w+\\[[^;\n]+\n?[^;\n]+\\]\\(?:.*/;.*\\|[^;]*\\)\\(?:\n[ \t:]+.*\\)?:=\\|Begin\\[\\|End\\[\\|EndPackage\\[") 
;; match foo[], foo[x_] := (not foo[x];), foo::usage, 
;; foo[x_(opt \n for long list)](opt /; bar(opt \n)) :=
(add-hook 'mma-mode-hook
  '(lambda ()
    (set (make-local-variable 'outline-regexp) mma-outline-regexp)))



(if (eq emacs-profile 'windows-1)
  (setq mathematica-command-line "C:/Program Files/Wolfram Research/Mathematica/7.0/math")
  (setq mathematica-command-line "/usr/local/bin/math")
)



;; Matlab mode:
;; old matlab mode stuff:
; (autoload 'matlab-mode "~/emacs.d/matlab.el" "Enter Matlab mode." t)
; (setq auto-mode-alist (cons '("\\.m\\'" . matlab-mode) auto-mode-alist))
; (autoload 'matlab-shell "~/emacs.d/matlab.el" "Interactive Matlab mode." t)
; (setq load-path (append load-path '("~/.emacs.d"))) 

;; Matlab mode stuff as per Matlab (windows) instructions:
;(add-to-list 'load-path "~/.emacs.d/elisp/matlab-emacs/") 

(setq load-path (cons "~/.emacs.d/elisp/matlab-emacs/" load-path))
;; NB: installation instructions that say (require 'matlab-load) are WRONG; 
;; use the following instead:
;; (load-file (expand-file-name 
;; 	    "~/.emacs.d/elisp/matlab-emacs/matlab-load.el"))
(load-library "matlab-load")
(setq-default matlab-show-mlint-warnings nil)
(setq-default matlab-highlight-cross-function-variables t)


(autoload 'matlab-eei-connect "matlab-eei" 
  "Connects Emacs to MATLAB's external editor interface.")

(autoload 'matlab-mode "matlab" "Enter Matlab mode." t)
(setq auto-mode-alist (cons '("\\.m\\'" . matlab-mode) auto-mode-alist))
(autoload 'matlab-shell "matlab" "Interactive Matlab mode." t)

(setq matlab-indent-function t)	; if you want function bodies indented
(setq matlab-verify-on-save-flag nil)	; turn off auto-verify on save


(defun my-matlab-eval ()
  (interactive)
  (matlab-shell-run-region-or-line)
  (deactivate-mark) ;; doesn't work, have to go manually modify matlab.el
  (matlab-show-matlab-shell-buffer))

(defun my-matlab-mode-hook ()
  (define-key matlab-mode-map (kbd "C-c r") 'my-matlab-eval)
  (define-key matlab-mode-map (kbd "C-c RET") 'my-matlab-eval)
  ;; (local-set-key (kbd "C-c RET") 'my-matlab-eval)
  (define-key matlab-mode-map [(shift return)] 'my-matlab-eval)
  (setq fill-column 77)
  (imenu-add-to-menubar "Find"))
(add-hook 'matlab-mode-hook 'my-matlab-mode-hook)


(add-hook 'matlab-shell-mode-hook
	  '(lambda()
	     (local-set-key [up] 'my-matlab-shell-previous-matching-input-from-input)
	     (local-set-key [down] 'my-matlab-shell-next-matching-input-from-input)
	     (local-set-key "\M-o" 'prev-input-goto-paren)))

; ~matlab-mode-stuff

;;}}}

;;{{{ LaTex/AucTeX settings

(if (not (eq emacs-profile 'linux-default))
(require 'tex-site)
(when (eq system-type 'windows-nt)
     (require 'tex-mik))
)

;;Anrei says forcing latex mode for tex files explicitly is better in some way
(setq auto-mode-alist (append '(("\\.tex$" . latex-mode))
			      auto-mode-alist))    

(add-hook 'TeX-mode-hook 'TeX-PDF-mode) ; NB: if already in TeX-PDF-mode
                            ; via some other magic, this will turn it OFF

(add-hook 'TeX-mode-hook 'auto-fill-mode) ; hook the auto-fill-mode with LaTeX-mode
(add-hook 'TeX-mode-hook 'outline-minor-mode) 
(add-hook 'TeX-mode-hook
	  '(lambda ()
	     (define-key TeX-mode-map [(control \\)] 'TeX-electric-macro)))


(require 'abbrev)
(setq save-abbrevs t) 
(add-hook 'TeX-mode-hook (lambda ()
     (setq abbrev-mode t)))

;; (add-hook 'outline-mode-hook
;; 	  '(lambda ()
;; 	     (require 'outline-mode-easy-bindings)))

;; (add-hook 'outline-minor-mode-hook
;; 	  '(lambda ()
;; 	     (require 'outline-mode-easy-bindings)))
 


(add-hook 'TeX-mode-hook '(lambda () (setq fill-column 77)))
;; (add-hook 'LaTeX-mode-hook '(lambda ()
;; 			      (setq outline-minor-mode-prefix "\C-c\C-o")))
(setq-default fill-column 77)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex) 
(setq reftex-plug-into-AUCTeX t)
(add-hook 'LaTeX-mode-hook (lambda ()
			     (TeX-fold-mode 1))) ;turn on
					;tex-fold-mode
					;by default
(add-hook 'LaTeX-mode-hook
	  (function (lambda ()
		      (setq TeX-auto-save t)
		      (setq TeX-parse-self t)
		      )))
   ; not necessary, I think....:
;; (add-hook 'LaTeX-mode-hook
;; 	  (function ( lambda()
;; 		      (setq TeX-command-default "LaTeX")
;; 		      )))

;; Tun on the flyspell mode
(defun turn-on-flyspell-mode () (flyspell-mode t))
;; disable spell chekcing of out commented lines
(setq ispell-check-comments nil)

(add-hook 'LaTex-mode-hook 'turn-on-flyspell-mode)
(add-hook 'TeX-mode-hook 'turn-on-flyspell-mode)
(add-hook 'html-mode-hook 'turn-on-flyspell-mode)

(add-hook 'LaTeX-mode-hook
	  (function (lambda ()
		      (setq ispell-parser 'tex)
		      )))

;;}}}~ end LaTeX/AucTeX customizations

;;{{{ ESS/R options ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Need to be careful - on some hosts ESS might be in ~/.emacs.d, on others in site-lisp
(require 'ess-site)
(setq ess-ask-for-ess-directory nil)
(setq ess-local-process-name "R")
(setq ansi-color-for-comint-mode 'filter)
;; (setq comint-prompt-read-only t)
(setq comint-scroll-to-bottom-on-input t)
(setq comint-scroll-to-bottom-on-output t)
(add-hook 'ess-mode-hook '(lambda () (setq comint-move-point-for-output t)))

;; Do not echo the evaluated commands into the transcript (R process window)
;; (the output is going to be displayed, however)
(setq  ess-eval-visibly-p nil)
(autoload 'ess-rdired "ess-rdired"
       "View *R* objects in a dired-like buffer." t)

;; This stuff (stolen from emacs wiki?) evaluates things via shift-return
(defun my-ess-start-R ()
  (interactive)
  (if (not (or (member "*R*" (mapcar (function buffer-name) (buffer-list)))
	       (member "*shell*" (mapcar (function buffer-name) (buffer-list)))))
      (progn
	(delete-other-windows)
	(setq w1 (selected-window))
	(setq w1name (buffer-name))
	(setq w2 (split-window w1))
	(R)
	(set-window-buffer w2 "*R*")
	(set-window-buffer w1 w1name))))

(defun my-ess-eval ()
  (interactive)
  (my-ess-start-R)
  (if (and transient-mark-mode mark-active)
      (call-interactively 'ess-eval-region-and-go)
    (call-interactively 'ess-eval-line-and-go)))

(add-hook 'ess-mode-hook
	  '(lambda()
	     (local-set-key [(shift return)] 'my-ess-eval)
	     (local-set-key [(control c) tab] 'ess-complete-object-name)))

(add-hook 'ess-mode-hook
	  '(lambda()
	     (setq fill-column 78)))

(defun my-matlab-shell-next-matching-input-from-input (n alt-action)
  "Get the Nth next matching input from for the command line
   unless we are at BOL in which case perform alt-action"
  (interactive "p")
  (my-matlab-shell-previous-matching-input-from-input (- n) alt-action))

;; my slight modification of Eric Ludlam's code from matlab.el
(defun my-matlab-shell-previous-matching-input-from-input (n alt-action)
  "Get the Nth previous matching input from for the command line,
   unless we are at BOL in which case perform alt-action"
  (interactive "p")
  (let ((start-point (point)) (at-bol nil))
    (save-excursion (comint-bol)
		    (if (eq start-point (point))
			(setq at-bol t)))
  (if (and (comint-after-pmark-p) (not at-bol))
      (if (memq last-command '(my-matlab-shell-previous-matching-input-from-input
			       my-matlab-shell-next-matching-input-from-input))
	  ;; This hack keeps the cycling working well. 
	  (let ((last-command 'comint-previous-matching-input-from-input))
	    (comint-next-matching-input-from-input (- n)))
	;; first time.
	(comint-next-matching-input-from-input (- n)))

    ;; If somewhere else, just move around.
    (funcall alt-action n))))

(defun my-matlab-shell-next-matching-input-from-input-prevline (n)
  (interactive "p")
  (my-matlab-shell-next-matching-input-from-input n 'previous-line))
(defun my-matlab-shell-previous-matching-input-from-input-prevline (n)
  (interactive "p")
  (my-matlab-shell-previous-matching-input-from-input n 'previous-line))
(defun my-matlab-shell-next-matching-input-from-input-previnput (n)
  (interactive "p")
  (my-matlab-shell-next-matching-input-from-input n 'comint-previous-input))
(defun my-matlab-shell-previous-matching-input-from-input-previnput (n)
  (interactive "p")
  (my-matlab-shell-previous-matching-input-from-input n 'comint-previous-input))



(add-hook 'inferior-ess-mode-hook
	  '(lambda()
	     ;; (local-set-key [C-up] 'comint-previous-matching-input-from-input)
	     (local-set-key [up] 'my-matlab-shell-previous-matching-input-from-input-prevline)
	     (local-set-key [down] 'my-matlab-shell-next-matching-input-from-input-prevline)
;;	     (define-key inferior-ess-mode-map "\M-o" 'prev-input-goto-paren)
	     (local-set-key "\M-o" 'prev-input-goto-paren)))

(defun my-kill-this-buffer ()
  (interactive) (kill-buffer (buffer-name)))
(add-hook 'ess-help-mode-hook
	  '(lambda()
	     (define-key ess-help-mode-map "q" 'my-kill-this-buffer)))

(defadvice ess-display-help-on-object (after ess-help-turn-off-viewmode () activate)
  "Turns off viewmode if it's on due to read-onlyness of the ESS help buffer"
  (setq view-mode nil))

;; Linking ESS with AucTex

(add-to-list 'auto-mode-alist '("\\.Rnw\\'" . Rnw-mode))
(add-to-list 'auto-mode-alist '("\\.Snw\\'" . Snw-mode))
(setq TeX-file-extensions
     '("Snw" "Rnw" "nw" "tex" "sty" "cls" "ltx" "texi" "texinfo"))
(add-hook 'Rnw-mode-hook
	  (lambda ()
	    (add-to-list 'TeX-command-list
			 '("Sweave" "R CMD Sweave %s"
			   TeX-run-command nil (latex-mode) :help "Run Sweave") t)
;			 '("LatexSweave" "%l %(mode) \\input{%s}"

	    (add-to-list 'TeX-command-list
			 '("LatexSweave" "%l %(mode) %s"
			   TeX-run-TeX nil (latex-mode) :help "Run Latex after Sweave") t)
	    (setq TeX-command-default "Sweave")))

(add-hook 'Rnw-mode-hook 'auto-fill-mode) ; hook the auto-fill-mode with LaTeX-mode
(add-hook 'Rnw-mode-hook '(lambda () (setq fill-column 77)))
(setq-default fill-column 77)

;;}}} END ESS/R options ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;dark room:
;; (require 'martin-darkroom)

;;{{{ text-processing functions: word counting, appending line numbers

(defun wc ()
  (interactive)
  (message "Word count: %s" (how-many "\\w+" (point-min) (point-max))))

(defun number-lines-region (start end &optional beg)
  (interactive "*r\np")
  (let* ((lines (count-lines start end))
	 (from (or beg 1))
	 (to (+ lines (1- from)))
	 (numbers (number-sequence from to))
	 (width (max (length (int-to-string lines))
		     (length (int-to-string from)))))
    (if (= start (point))
	(setq numbers (reverse numbers)))
    (goto-char start)
    (dolist (n numbers)
      (beginning-of-line)
      (save-match-data
	(if (looking-at " *-?[0-9]+\\. ")
	    (replace-match "")))
      (insert (format (concat "%" (int-to-string width) "d. ") n))
      (forward-line))))

;;}}}

;;  Allow ido to open recent files
(require 'recentf)
(setq recentf-exclude '(".ftp:.*" ".sudo:.*" ".*\.recentf" ".*\.ido.last"))
(setq recentf-keep '(file-remote-p file-readable-p))
(setq recentf-exclude '("\\.ido\\.last" "\\.recentf"))
(recentf-mode 1)
(setq recentf-max-saved-items 500)
(setq recentf-max-menu-items 60)

(defvar lva-quick-file-1-fname 
  (lva-get-first-matching-string lva-quick-file-1 recentf-list))
(defvar lva-quick-file-2-fname 
  (lva-get-first-matching-string lva-quick-file-2 recentf-list)) 
(defvar lva-quick-file-3-fname 
  (lva-get-first-matching-string lva-quick-file-3 recentf-list))  
(global-set-key "\C-c1" '(lambda () (interactive) (find-file lva-quick-file-1-fname)))
(global-set-key "\C-c2" '(lambda () (interactive) (find-file lva-quick-file-2-fname)))
(global-set-key "\C-c3" '(lambda () (interactive) (find-file lva-quick-file-3-fname)))

;; uniquify settings
(require 'uniquify)
(add-hook 'eshell-post-command-hook 'eshell-dir-buffer-name)

(defun eshell-rename-buffer (x)
  (rename-buffer
   (concat (car (split-string (buffer-name) "|")) "|" x)
   t))

(defun eshell-dir-buffer-name () (eshell-rename-buffer default-directory))


(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "|")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")
;;~ end uniqify settings

;; Change title bar to ~/file-directory if the current buffer is a
;; real file or buffer name if it is just a buffer.
;; (setq frame-title-format 
;;       '(:eval 
;;         (if buffer-file-name 
;;             (replace-regexp-in-string (getenv "HOME") "~" 
;;                                       (file-name-directory buffer-file-name)) 
;;           (buffer-name))))

;(setq frame-title-format (concat invocation-name "@" system-name ": %b %+%+ %f"))
(setq frame-title-format (concat invocation-name ": %b %+%+ %f"))

   ; (setq frame-title-format "%b") ; simpler code for setting frame title
;;~ end rename title bar

;; icicles mode:
;(setq load-path (cons "~/.emacs.d/elisp/icicles/" load-path))


;;{{{ ido settings (incl keymap, ido recentf, compl. read defadvice):

(require 'ido)
;; prevent ido from running ido-wash-history when doing sudo (this doesn't play well with tramp)
(defadvice ido-wash-history (around dont-run-if-root activate)
  (message "IWH advice"))
  ;; (unless (string= (getenv "USER") "root") ad-do-it))
(ido-mode t)
(setq ido-enable-flex-matching t)
(setq ido-create-new-buffer 'no-prompt)
(ido-everywhere t)
(setq ido-max-prospects 15)

(add-hook 'ido-setup-hook 'ido-my-keys)

(defun ido-my-keys ()
 "Add my keybindings for ido."
    (define-key ido-completion-map [(control tab)] 'ido-next-match)
    (define-key ido-completion-map "`" 'ido-exit-minibuffer)
    (define-key ido-completion-map "\t" 'ido-next-match)
    (define-key ido-completion-map [(control ?`)] '(lambda()(interactive)(insert "`")))
    (define-key ido-completion-map "\C-n" 'ido-next-match)
    (define-key ido-completion-map "\C-p" 'ido-prev-match)
    (define-key ido-completion-map [(shift tab)] 'ido-prev-match)
    (define-key ido-completion-map [backtab] 'ido-prev-match)
 )


;; (defvar ido-execute-command-cache nil)

;; (defun ido-execute ()
;;   (interactive)
;;   (let ((ido-max-prospects 7))
;;   (call-interactively
;;    (intern
;;     (ido-completing-read
;;      "M-x "
;;      (progn
;;        (unless ido-execute-command-cache
;; 	 (mapatoms (lambda (s)
;; 		     (when (commandp s)
;; 		       (setq ido-execute-command-cache
;; 			     (cons (format "%S" s) ido-execute-command-cache)))))
;; 	 (setq ido-execute-command-cache 
;; 	       (sort (sort ido-execute-command-cache 'string-lessp)
;; 		     (lambda (x y) (< (length x) (length y)))))
;; 	 )
;;        ido-execute-command-cache))))))

;; (defun ido-update-mx ()
;;   (setq ido-execute-command-cache nil)
;; )

(defadvice completing-read
       (around foo activate)
       (if (boundp 'ido-cur-item)
           ad-do-it
         (setq ad-return-value
               (ido-completing-read
                prompt
                (all-completions "" collection predicate)
                nil require-match initial-input hist def))))




;; (defun ido-execute ()
;;  (interactive)
;;  (let ((ido-max-prospects 7))
;;  (call-interactively
;;   (intern
;;    (ido-completing-read
;;     "M-x "
;;     (let (cmd-list)
;;       (mapatoms (lambda (S)
;;                   (when (commandp S)
;;                     (setq cmd-list (cons (format "%S" S) cmd-list)))))
;;       (sort (sort cmd-list 'string-lessp)
;;             (lambda (x y) (< (length x) (length y))))))))))
;; (global-set-key "\M-x" 'ido-execute)
;; (global-set-key "\C-x\C-m" 'ido-execute)
;; (global-set-key "\C-c\C-m" 'ido-execute)


(defun ido-choose-from-recentf ()
 "Use ido to select a recently opened file from the `recentf-list'"
 (interactive)
 (let ((home (expand-file-name (getenv "HOME"))))
   (find-file
    (ido-completing-read "Recentf open: "
                         (mapcar (lambda (path)
                                   (replace-regexp-in-string home "~" path))
                                 recentf-list)
                         nil t))))

(global-set-key (kbd "C-x f") 'ido-choose-from-recentf)
(add-hook 'find-file-hook '(lambda () (progn (recentf-save-list)
						 (message nil))))
;;~ end set ido to do recent files


;;~ end ido-related stuff

;;}}}

;;this functionality is superceded by smex:
(add-to-list 'load-path (expand-file-name "~/.emacs.d/elisp/smex.git"))
(require 'smex)
;  (smex-initialize) is put at the end of .emacs
(setq smex-save-file "~/.emacs.d/.smex-items")
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c M-x") 'smex-update-and-run)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)





;; --- Quick bookmarks -----------------------------------------------------
(require 'af-bookmarks)
(global-set-key [(control f2)]  'af-bookmark-toggle )
(global-set-key "\C-cb"  'af-bookmark-toggle )
(global-set-key [f2]  'af-bookmark-cycle-forward )
(global-set-key [(shift f2)]  'af-bookmark-cycle-reverse )
(global-set-key [(control shift f2)]  'af-bookmark-clear-all )
(global-set-key "\C-c\C-b"  'af-bookmark-clear-all )
(global-set-key "\C-ccb"  'af-bookmark-clear-all )



;;{{{ --- Breadcrumb settings ---------------------------------------------------

;(require 'breadcrumb)
;; (autoload 'bc-set "breadcrumb" "Set bookmark in current point." t)
;; (autoload 'bc-previous "breadcrumb" "Go to previous bookmark." t)
;; (autoload 'bc-next "breadcrumb" "Go to next bookmark." t)
;; (autoload 'bc-local-previous "breadcrumb" "Go to previous local bookmark." t)
;; (autoload 'bc-local-next "breadcrumb" "Go to next local bookmark." t)
;; (autoload 'bc-goto-current "breadcrumb" "Go to the current bookmark." t)
;; (autoload 'bc-list "breadcrumb" "List all bookmarks in menu mode." t)
;; (autoload 'bc-clear "breadcrumb" "Clear all bookmarks." t) 
;;   ; breadcrumb keys:
;; ; (global-set-key (kbd "S-SPC")         'bc-set)  ;; used to be S-M-SPC
;; (global-set-key (kbd "C-S-SPC") 'bc-set)
;; (global-set-key (kbd "C-S-<left>") 'bc-previous)
;; (global-set-key (kbd "C-S-<right>") 'bc-next)
;; (global-set-key (kbd "C-S-<down>") 'bc-local-previous)
;; (global-set-key (kbd "C-S-<up>") 'bc-local-next)
;; (global-set-key (kbd "C-S-<return>") 'bc-goto-current)
;; (global-set-key (kbd "C-S-l") 'bc-list)
;; (global-set-key (kbd "C-S-c") 'bc-clear)

;;(global-set-key [f2]         'bc-set) 
;; Shift-SPACE for set bookmark
;; (global-set-key [(meta j)]              'bc-previous)       ;; M-j for jump to previous
;; (global-set-key [(shift meta j)]        'bc-next)           ;; Shift-M-j for jump to next
;; (global-set-key [(meta up)]             'bc-local-previous) ;; M-up-arrow for local previous
;; (global-set-key [(meta down)]           'bc-local-next)     ;; M-down-arrow for local next
;; (global-set-key [(control c)(j)]        'bc-goto-current)   ;; C-c j for jump to current bookmark
;; (global-set-key [(control x)(meta j)]   'bc-list)           ;; C-x M-j for the bookmark menu list

;;}}}~end Breadcrumb settings ---------------------------------------------------

;; Add color to a shell running in emacs 'M-x shell'
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)



;;{{{ -- Andrei's magic line-dragging code --------------------------------------

(defun move-line (&optional n)
 "Move current line N (1) lines up/down leaving point in place."
 (interactive "p")
 (when (null n)
   (setq n 1))
 (let ((col (current-column)) 
       (line-move-visual nil))
   (interactive)
   (beginning-of-line)
   (next-line 1)
   (transpose-lines n)
   (previous-line 1)
   (move-to-column col)))

(defun move-line-up (n)
 "Moves current line N (1) lines up leaving point in place."
 (interactive "p")
 (move-line (if (null n) -1 (- n))))

(defun move-line-down (n)
 "Moves current line N (1) lines down leaving point in place."
 (interactive "p")
 (move-line (if (null n) 1 n)))

(global-set-key (kbd "S-M-<down>") 'move-line-down) 
(global-set-key (kbd "S-M-<up>") 'move-line-up)

;;}}}~end Andrei's magic line-dragging code -------------------------------------

;; -- Jump by n lines up/down:
(defun jump-forward-lines()
   " This function will move the cursor forward some lines (currently 10)."
   (interactive)
   (forward-line 5))
(defun jump-back-lines()
   " This function will move the cursor back a few lines (currently 10)."
   (interactive)
   (forward-line -5))
(global-set-key (kbd "M-<down>") 'jump-forward-lines)
(global-set-key (kbd "M-<up>") 'jump-back-lines)



;; make buffers focus when they are displayed in another frame 
;; (i.e. make the display-buffer and pop-to-buffer ical in functionality
;; (defadvice display-buffer (after display-buffer-focus activate compile)
;; "Focuses the buffer after switching to it, mimicking pop-to-buffer"
;; (other-window 1)
;; )
;(setq pop-up-frames nil)
;(setq pop-up-windows t)


;;{{{ -- kill/yank enhancements -------------------------

(when (require 'browse-kill-ring nil 'noerror)
  (browse-kill-ring-default-keybindings))

(global-set-key "\C-cy" '(lambda ()
   (interactive)
   (popup-menu 'yank-menu)))

;; enable killing/copying lines w/o having them marked
;; cf http://www.emacswiki.org/emacs/SlickCopy
(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (message "Copied line")
     (list (line-beginning-position)
	   (line-beginning-position 2)))))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
	   (line-beginning-position 2)))))

;; Author: Eberhard Mattes <mattes@azu.informatik.uni-stuttgart.de>
(defun emx-duplicate-current-line (arg)
  "Duplicate current line.
Set mark to the beginning of the new line.
With argument, do this that many times."
  (interactive "*p")
  (setq last-command 'identity) ; Don't append to kill ring
  (let ((s (point)))
    (beginning-of-line)
    (let ((b (point)))
      (forward-line)
      (if (not (eq (preceding-char) ?\n)) (insert ?\n))
      (copy-region-as-kill b (point))
    (while (> arg 0)
      (yank)
      (setq arg (1- arg)))
    (goto-char s))))

(defun djcb-duplicate-line (&optional commentfirst)
  "comment line at point; if COMMENTFIRST is non-nil, comment the original" 
  (interactive)
  (beginning-of-line)
  (push-mark)
  (end-of-line)
  (let ((str (buffer-substring (region-beginning) (region-end))))
    (when commentfirst
    (comment-region (region-beginning) (region-end)))
    (insert-string
      (concat (if (= 0 (forward-line 1)) "" "\n") str "\n"))
    (forward-line -1)))



(defun duplicate-current-region ()
  (interactive)
  (copy-region-as-kill (region-beginning) (region-end))
  (yank)
  (back-to-indentation))

(global-set-key (kbd "C-c d") 'emx-duplicate-current-line) ; or dup + comment:
(global-set-key (kbd "C-c C-d") (lambda()(interactive)(djcb-duplicate-line t)))
;; (global-set-key "\C-cd" 'emx-duplicate-current-line)
(global-set-key (kbd "s-w") 'duplicate-current-line)
(global-set-key (kbd "s-k") 'kill-ring-save)

;;}}}~end enable killing/copying lines w/o having them marked -------------------

;;------ compilation: 'recompile and 'compile bound to f-keys
;;{{{ -- compilation stuff  -----------------------------------------------------

(defun save-windows-recompile () "Recompiling"
  (proc
   (window-configuration-to-register ?w)
   (recompile))
  )
(global-set-key [f9] 'recompile)
(global-set-key [f10] 'compile)


;; ;; Helper for compilation. Close the compilation window if
;; ;; there was no error at all.
;; (defun compilation-exit-autoclose (status code msg)
;;   ;; If M-x compile exists with a 0
;;   (when (and (eq status 'exit) (zerop code))
;;     ;; then bury the *compilation* buffer, so that C-x b doesn't go there
;;     (bury-buffer)
;;     ;; and delete the *compilation* window
;;     (delete-window (get-buffer-window (get-buffer "*compilation*"))))
;;   ;; Always return the anticipated result of compilation-exit-message-function
;;   (cons msg code))
;; ;; Specify my function (maybe I should have done a lambda function)
;; (setq compilation-exit-message-function 'compilation-exit-autoclose)


;; (setq compilation-finish-functions 'compile-autoclose)
;; (defun compile-autoclose (buffer string)
;;   (cond ((string-match "finished" string)
;; 	 (bury-buffer "*compilation*")
;; 	 ;;(winner-undo)
;; 	 (jump-to-register ?w)
;; 	 (message "Build successful."))
;; 	(t                                                                    
;; 	 (message "Compilation exited abnormally: %s" string))))

;;}}}

;; Line-wrapping stuff: ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; disable line wrap
;;(setq default-truncate-lines t)

;; make side by side buffers function the same as the main window
(setq truncate-partial-width-windows nil) ;; didn't work the first few times?
;;(setq truncate-lines nil)  ;; had to play w/ it before partial width worked

;; Optionally add key to toggle line wrap
;; (global-set-key [f10] 'toggle-truncate-lines)



;; Macros:
(fset 'paste-BOL
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ("" 0 "%d")) arg)))
(global-set-key "\C-c\C-p" 'paste-BOL)
(global-set-key "\C-cpb" 'paste-BOL)
(fset 'paste-EOL
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ("" 0 "%d")) arg)))
(global-set-key "\C-cpe" 'paste-EOL)
(fset 'square-parens-and-sincos
   [?\M-% ?\[ return ?\( return ?! up up up up up up up ?\M-< ?\M-% ?\] return ?\) return ?! ?\M-< ?\M-% ?C ?o ?s return ?c ?o ?s return ?! ?\M-< ?\M-% ?S ?i ?n return ?s ?i ?n return ?! ?\M-< ?\M-% ?G backspace])
(fset 'quote-list
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([201326624 134217766 34 34 return C-right C-left] 0 "%d")) arg)))
(global-set-key "\C-cmq" 'quote-list)
(fset 'prev-input-goto-paren
   [?\M-p ?\C-a ?\C-s ?\( ?\C-m left])
;;(global-set-key "\M-o" 'prev-input-goto-paren)



;; TODO: figure out how to write this w/o all the copypasting
(defvar TeX-output-view-style-commands)
(if (eq system-type 'windows-nt)
    (setq TeX-output-view-style-commands 
	  (quote (("^dvi$" "^pstricks$\\|^pst-\\|^psfrag$" "dvips %d -o && start \"\" %f") ("^dvi$" "." "yap -1 %dS %d") ("^pdf$" "." "'C:/Program Files/Ghostgum/gsview/gsview32.exe' %o") ("^html?$" "." "start \"\" %o")))
	  )
    (setq TeX-output-view-style-commands 
	  (quote (("^dvi$" "^pstricks$\\|^pst-\\|^psfrag$" "dvips %d -o && start \"\" %f") ("^dvi$" "." "yap -1 %dS %d") ("^pdf$" "." "'evince' %o") ("^html?$" "." "start \"\" %o")))
	  )
)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(TeX-electric-escape nil)
 '(TeX-output-view-style TeX-output-view-style-commands t)
 '(color-theme-is-cumulative t)
 '(cua-delete-selection nil)
 '(cua-enable-cua-keys nil)
 '(cua-remap-control-v nil)
 '(cua-remap-control-z nil)
 '(cygwin-mount-cygwin-bin-directory "c:\\cygwin\\bin")
 '(ecb-options-version "2.40")
 '(ess-eval-deactivate-mark t)
 '(ess-r-args-show-as (quote tooltip))
 '(grep-command "grep -nHi ")
 '(help-window-select t)
 '(hideshowvis-ignore-same-line nil)
 '(matlab-fill-fudge-hard-maximum 89)
 '(mlint-programs (quote ("mlint" "win32/mlint" "C:\\Program Files\\MATLAB\\R2008b\\bin\\win32\\mlint.exe" "/opt/matlab/R2009a/bin/glnxa64/mlint")))
 '(org-agenda-files (quote ("c:/Work/Dipole Problem/dipole.org" "~/My Dropbox/notes.org/memos.txt")))
 '(org-cycle-include-plain-lists nil)
 '(org-drawers (quote ("PROPERTIES" "CLOCK" "LOGBOOK" "CODE" "DETAILS")))
 '(org-hide-leading-stars t)
 '(org-replace-disputed-keys t)
 '(preview-transparent-color nil)
 '(quack-programs (quote ("C:/Program Files/MzScheme/mzscheme" "bigloo" "csi" "csi -hygienic" "gosh" "gsi" "gsi ~~/syntax-case.scm -" "guile" "kawa" "mit-scheme" "mred -z" "mzscheme" "mzscheme -il r6rs" "mzscheme -il typed-scheme" "mzscheme -M errortrace" "mzscheme3m" "mzschemecgc" "rs" "scheme" "scheme48" "scsh" "sisc" "stklos" "sxi")))
 '(safe-local-variable-values (quote ((TeX-auto-save . t) (TeX-parse-self . t) (folded-file . t))))
 '(scroll-preserve-screen-position 1)
 '(set-mark-command-repeat-pop 1)
 '(smooth-scroll-margin 5)
 '(speedbar-show-unknown-files t)
 '(temp-buffer-show-function (quote pop-to-buffer))
 '(thing-types (quote ("word" "symbol" "sexp" "list" "line" "paragraph" "page" "defun" "number" "form")))
 '(w32-symlinks-handle-shortcuts t)
 '(winner-ring-size 100)
 '(x-select-enable-clipboard t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(hs-face ((nil (:box nil)))))

(put 'narrow-to-region 'disabled nil)



; ----- init smex ---------
(smex-initialize)
; -------------------------

; (require 'frame-restore) ; don't work for me


;; Local variables:
;; folded-file: t
;; end:
