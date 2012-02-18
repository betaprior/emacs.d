;;; init.el --- Where all the magic begins
;;
;; This file loads Org-mode and then loads the rest of our Emacs initialization from Emacs lisp
;; embedded in literate Org-mode files.

;; Load up Org Mode and (now included) Org Babel for elisp embedded in Org Mode files
(setq dotfiles-dir (file-name-directory (or (buffer-file-name) load-file-name)))
(defvar debug-lp)
(let* ((org-dir (expand-file-name
                 "lisp" (expand-file-name
                         "org-mode.git" (expand-file-name
                                "elisp" dotfiles-dir))))
       (org-contrib-dir (expand-file-name
                         "lisp" (expand-file-name
                                 "contrib" (expand-file-name
                                            ".." org-dir))))
       (load-path (append 
		   (list org-dir org-contrib-dir) (or load-path nil))))
  (setq debug-lp load-path)
  ;; this must be done before org loads to guarantee that windmove works
  (setq org-disputed-keys '(([(shift up)] . [(meta p)])
			    ([(shift down)] . [(meta n)])
			    ([(shift left)] . [(meta -)])
			    ([(shift right)] . [(meta +)])
			    ([(meta return)] . [(control meta return)])
			    ([(control shift right)] . [(meta shift +)])
			    ([(control shift left)] . [(meta shift -)])))
  (setq org-replace-disputed-keys t)
  ;; load up Org-mode and Org-babel
  (require 'org-install)
  (require 'ob-tangle))

;; load up all literate org-mode files in this directory
(mapc #'org-babel-load-file (directory-files dotfiles-dir t "\\.org$"))
;; (load-file "/home/leo/.emacs.d/init.el.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-electric-escape nil)
 '(TeX-output-view-style TeX-output-view-style-commands)
 '(TeX-view-program-list (quote (("((\"Ghostview\" \"'C:/Program Files/Ghostgum/gsview/gsview32.exe' %o\"))" ""))) t)
 '(completion-ignored-extensions (quote (".o" "~" ".bin" ".lbin" ".so" ".a" ".ln" ".blg" ".bbl" ".elc" ".lof" ".glo" ".idx" ".lot" "CVS/" "_darcs/" "_MTN/" ".fmt" ".tfm" ".class" ".fas" ".lib" ".mem" ".x86f" ".sparcf" ".dfsl" ".pfsl" ".d64fsl" ".p64fsl" ".lx64fsl" ".lx32fsl" ".dx64fsl" ".dx32fsl" ".fx64fsl" ".fx32fsl" ".sx64fsl" ".sx32fsl" ".wx64fsl" ".wx32fsl" ".fasl" ".ufsl" ".fsl" ".dxl" ".lo" ".la" ".gmo" ".mo" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".vr" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc" ".pyo")))
 '(cua-delete-selection nil)
 '(cua-enable-cua-keys nil)
 '(cua-remap-control-v nil)
 '(cua-remap-control-z nil)
 '(cygwin-mount-cygwin-bin-directory "c:\\cygwin\\bin")
 '(doc-view-ghostscript-program "c:/cygwin/bin/gs.exe")
 '(ebib-index-window-size 20)
 '(ebib-layout (quote custom))
 '(ebib-width 55)
 '(ecb-options-version "2.40")
 '(ess-eval-deactivate-mark t)
 '(ess-r-args-show-as (quote tooltip))
 '(evil-cross-lines t)
 '(evil-flash-delay 10)
 '(font-lock-maximum-decoration (quote ((dired-mode) (t . t))))
 '(grep-command "grep -nHi ")
 '(grep-o-matic-ask-about-save nil)
 '(grep-o-matic-search-patterns (quote ("*.cpp" "*.c" "*.h" "*.awk" "*.sh" "*.py" "*.pl" "[Mm]akefile" "*.el" "*")))
 '(help-window-select t)
 '(hideshowvis-ignore-same-line nil)
 '(initial-scratch-message nil)
 '(line-move-visual nil)
 '(matlab-fill-fudge-hard-maximum 89)
 '(message-log-max 1000)
 '(mlint-programs (quote ("mlint" "win32/mlint" "C:\\Program Files\\MATLAB\\R2008b\\bin\\win32\\mlint.exe" "/opt/matlab/R2009a/bin/glnxa64/mlint")))
 '(preview-transparent-color nil)
 '(quack-programs (quote ("C:/Program Files/MzScheme/mzscheme" "bigloo" "csi" "csi -hygienic" "gosh" "gsi" "gsi ~~/syntax-case.scm -" "guile" "kawa" "mit-scheme" "mred -z" "mzscheme" "mzscheme -il r6rs" "mzscheme -il typed-scheme" "mzscheme -M errortrace" "mzscheme3m" "mzschemecgc" "rs" "scheme" "scheme48" "scsh" "sisc" "stklos" "sxi")))
 '(safe-local-variable-values (quote ((TeX-auto-save . t) (TeX-parse-self . t) (folded-file . t))))
 '(same-window-regexps (quote ("\\*rsh-[^-]*\\*\\(\\|<[0-9]*>\\)" "\\*telnet-.*\\*\\(\\|<[0-9]+>\\)" "^\\*rlogin-.*\\*\\(\\|<[0-9]+>\\)" "\\*info\\*\\(\\|<[0-9]+>\\)" "\\*gud-.*\\*\\(\\|<[0-9]+>\\)" "\\`\\*Customiz.*\\*\\'" "\\*shell.*\\*\\(\\|<[0-9]+>\\)")))
 '(scroll-preserve-screen-position 1)
 '(search-whitespace-regexp "[ 	

]+")
 '(set-mark-command-repeat-pop 1)
 '(smooth-scroll-margin 5)
 '(speedbar-show-unknown-files t)
 '(temp-buffer-show-function (quote pop-to-buffer))
 '(thing-types (quote ("word" "symbol" "sexp" "list" "line" "paragraph" "page" "defun" "number" "form")))
 '(undo-tree-mode-lighter "")
 '(visual-line-fringe-indicators (quote (nil right-curly-arrow)))
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


(put 'autopair-newline 'disabled nil)
(put 'downcase-region 'disabled nil)

;;; init.el ends here

