;; ###----startup performance----###

;; make startup faster by reducing the frequency of garbage collection and then use a hook to measure Emacs startup time. 
;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

;;________________________________________________________________
;;    Auto Completion
;;________________________________________________________________
(require 'auto-complete)
(global-auto-complete-mode t)
;; (setq ac-modes '(sh-mode lisp-mode c-mode c++-mode sql-mode html-mode)) ; you can specified only for some certain mode

;; Remember and restore the last cursor location of opened files
(save-place-mode 1)
(setq save-place-file (locate-user-emacs-file "places" ".emacs-places"))

;; may be running afoul of save-place-forget-unreadable-files.
;; On exit, it checks that every loaded file is readable before
;; saving its buffer position. potentially very slow if you use NFS.
;; bellow cmnd will restores emacs exit to nearly instantaneous.
;; (setq save-place-forget-unreadable-files nil)

;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)

;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t)

;;________________________________________________________________
;;    Separte Customization from init file
;;________________________________________________________________
;; It will keep the user customization created with
;; M-x customize-theme, M-x customize-group in a separate file ~/.emacs.d/custom.el.

;; Move customization variables to a separate file and load it
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

;;________________________________________________________________
;;    Modeline
;;________________________________________________________________

;; Basic Customization
(setq display-time-format "%l:%M%P (%a) %e %b ♪" ;; %D for date format
      display-time-default-load-average nil)

;;  (setq mode-line-compact t)
(setq line-move-visual t)
;;(setq indent-tabs-mode t)

;; Permanent display of line and column numbers is handy.
(setq-default line-number-mode 't)
(setq-default column-number-mode 't)
(display-time-mode)

(global-visual-line-mode 1)

;;________________________________________________________________
;;    Balancing Parentheses
;;________________________________________________________________

(setq show-paren-delay 0)           ; how long to wait?
(show-paren-mode 1)                 ; turn paren-mode on
(setq show-paren-style 'mixed)      ; alternatives are 'expression' and 'parenthesis'

;; auto close bracket insertion. New in emacs 24
(electric-pair-mode 1)
;; make electric-pair-mode work on more brackets
(setq electric-pair-pairs
      '(
        (?\" . ?\")
        (?\< . ?\>)
        (?\{ . ?\})))

;;________________________________________________________________
;;    Editing Related
;;________________________________________________________________

;; make typing delete/overwrites selected text
(delete-selection-mode 1)

;; make return key also do indent, for current buffer only
(electric-indent-local-mode 1)

;; make return key also do indent, globally ;; indentation, end of line
(electric-indent-mode 1)
(setq-default electric-indent-inhibit t)

;; Backspacing over a tab, just delete the tab:
(setq-default c-backspace-function 'backward-delete-char)

;;________________________________________________________________
;;    Global Key Bindings
;;________________________________________________________________

; Making global key bindings in Emacs Lisp (example):
;; (global-set-key (kbd "C-c g") 'search-forward)

(global-set-key (kbd "M-#") 'query-replace-regexp)

;; To make sure that emacs is actually using tabs instead of spaces:
(global-set-key (kbd "TAB") 'self-insert-command)

;; Duplicate a whole line
(global-set-key "\C-c\C-d" "\C-a\C- \C-n\M-w\C-y")

;; ###----Setting Key Bindings with use-package----###

;; a minor mode that records your window configurations
;; and lets you undo and redo changes made to it.

(use-package winner
  :config
  (winner-mode 1)
  :bind (("M-[" . winner-undo)
         ("M-]" . winner-redo)))


(add-hook 'text-mode-hook (lambda ( ) (refill-mode 1)))
(global-set-key (kbd "C-<f1>")
                (lambda () (interactive)
                  (load-theme 'zenburn t)))

;; call-last-kbd-macro frequently used key on a double key sequence (I think original is ^Xe). I've also redefined ESC-&.
;; (global-set-key "\C-z" 'call-last-kbd-macro)
;; (define-key esc-map "&" 'query-replace-regexp)

;; You can also define your own function. e.g:

;; (defun duplicate-line()
;;   (interactive)
;;   (move-beginning-of-line 1)
;;   (kill-line)
;;   (yank)
;;   (open-line 1)
;;   (next-line 1)
;;   (yank) )
;; (global-set-key (kbd "C-d") 'duplicate-line) 

;; (defun my-custom-settings-fn ()
;;   (setq tab-width 4)
;;   (setq indent-tabs-mode t)
;;   (setq tab-stop-list (number-sequence 4 200 4)))
;; (add-hook 'text-mode-hook 'my-custom-settings-fn)

(defun my-c-mode-common-hook ()
  "my customizations for all of c-mode and related modes"
  (setq c-basic-offset 4)
  (setq c-basic-indent 4))

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;; Space / Tabs - Indentation

;;(setq tab-width 4 indent-tabs-mode nil)			; set indentation with spaces instead of tabs with 4 spaces
;;(setq-default tab-width 4)						; set default tab char's display width to 4 spaces
;;(setq-default indent-tabs-mode nil)
;;(setq-default tab-always-indent nil)			; force emacs to always use spaces instead of tab characters
(setq-default tab-always-indent 'complete)		; make tab key do indent first then completion
(setq sentence-end-double-space nil)
;; (setq-default tab-always-indent t)			; make tab key always call a indent command

;;find-file-at-point, smarter C-x C-f when point on path or URL
(ffap-bindings)

;; Unique buffer name
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Show recent files in the File menu.
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-saved-items 500)
(setq recentf-max-menu-items 60)

;; Saves the minibuffer history on every Emacs session.
(savehist-mode 1)
(setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
(setq history-length 25)

;; To set the file which the minibuffer is saved use:
(setq savehist-file "~/.emacs.d/tmp/savehist")
;;________________________________________________________________
;;    Display Bars
;;________________________________________________________________

(setq inhibit-startup-screen t)			; Turn-off Startup en
(setq initial-scratch-message nil)		; Don't insert instructions in the *scratch* buffer
(setq use-dialog-box nil)				; Don't pop up UI dialogs when prompting
(defalias 'yes-or-no-p 'y-or-n-p)		; Ask y or n instead of yes or no
;; (fset 'yes-or-no-p 'y-or-n-p)		; Change all prompts to y or n
(setq-default visible-bell t)			; Flash the screen on error, don't beep
;; (setq ring-bell-function #'ignore)	; Turn-off Alarm Bell
(setq-default view-read-only t)		 	; Toggle ON or OFF with M-x view-mode (or use e to exit view-mode).

;; Disable some default feature
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(load-theme 'tango-dark t)
;; (set-foreground-color "ivory")
;; (set-background-color "darkblue")
(set-face-attribute 'region nil :background "#666" :foreground "#ffffff")	; Change the HIGHLIGHT COLOR for SELECTED TEXT
;; (set-face-attribute 'highlight nil :foreground 'unspecified)
;; The built-in view-mode turns the current buffer into a read-only pager.

(setq-default display-line-numbers 'relative)
(setq-default display-line-numbers-width 2)
(set-frame-parameter nil 'fullscreen 'fullheight)
;;________________________________________________________________
;;    Fonts Setting
;;________________________________________________________________

(global-font-lock-mode 1)				; Use font-lock everywhere.
(setq font-lock-maximum-decoration t)	; We have CPU to spare; highlight all syntax categories.

(set-frame-font "Comic Mono-10.5" nil t)
;; (set-frame-font "Monaco-9" nil t)
;; (set-frame-font "Fantasque Sans Mono-10.5" nil t)
;; (set-frame-font "Source Code Pro-10" nil t)
;; (set-frame-font "Fira Code-10" nil t)

;;________________________________________________________________
;;    Cursor Mode
;;________________________________________________________________

;; Set cursor and mouse colours:
(set-cursor-color "Orchid3")
(set-mouse-color "white")
(setq x-stretch-cursor t)	; make cursor the width of the character it is under i.e. full width of a TAB
(setq-default cursor-type 'box)
(blink-cursor-mode -1)		; Turn-off Cursor Blink (1 to Enable & 0 to Stop)
;;________________________________________________________________
;;    Backup Files
;;________________________________________________________________

(defvar --backup-directory (concat user-emacs-directory "backups"))
(if (not (file-exists-p --backup-directory))
        (make-directory --backup-directory t))
(setq backup-directory-alist `(("." . ,--backup-directory)))
(setq make-backup-files t               ; backup of a file the first time it is saved.
      backup-by-copying t               ; don't clobber symlinks
      version-control t                 ; version numbers for backup files
      delete-old-versions t             ; delete excess backup files silently
      delete-by-moving-to-trash t
      kept-old-versions 2               ; oldest versions to keep when a new numbered backup is made (default: 2)
      kept-new-versions 6               ; newest versions to keep when a new numbered backup is made (default: 2)
      auto-save-default t               ; auto-save every buffer that visits a file
      auto-save-timeout 30              ; number of seconds idle time before auto-save (default: 30)
      auto-save-interval 200            ; number of keystrokes between auto-saves (default: 300)
      )
(setq delete-auto-save-files t)			; deletes buffer's auto save file when it is saved or killed with no changes in it.

;; Automatically purge backup files not accessed in a week:
(message "Deleting old backup files...")
(let ((week (* 60 60 24 7))
      (current (float-time (current-time))))
  (dolist (file (directory-files temporary-file-directory t))
    (when (and (backup-file-name-p file)
               (> (- current (float-time (fifth (file-attributes file))))
                  week))
      (message "%s" file)
      (delete-file file))))

;;________________________________________________________________
;; 	Identity Who I Am ?   
;;________________________________________________________________

;; Do not publish my email on Usenet
(setq user-full-name       "Likhon Barai"
      user-login-name      "Likhon-art"
      user-real-login-name "Likhon"
      user-mail-address    "likhonhere007@gmail.com"
      system-name          "Art.Likhon")
;;________________________________________________________________
;;    Highlight Current LINE
;;________________________________________________________________
(global-hl-line-mode 1)
(set-face-background 'highlight "#3e4446")	; you canalso try: "#3e4446" or "#gray6" or etc.
(set-face-foreground 'highlight nil)
;; (set-face-underline-p 'highlight "#ff0000")
 
;; (vline-global-mode 1)
;; (set-face-background 'vline "#ff0000")	; you canalso try: "#3e4446" or "#gray6" or etc.
;; (set-face-foreground 'vline nil)
;; (setq vline-style 'mixed)

;;________________________________________________________________
;;    Transparent Emacs
;;________________________________________________________________
(set-frame-parameter (selected-frame) 'alpha '(85 . 50))
(add-to-list 'default-frame-alist '(alpha . (85 . 50)))
;; (set-frame-parameter (selected-frame) 'alpha '(<active> . <inactive>))
;; (set-frame-parameter (selected-frame) 'alpha <both>)

;; Use the following snippet after you’ve set the alpha as above to assign a toggle to “C-c t”:
 (defun toggle-transparency ()
   (interactive)
   (let ((alpha (frame-parameter nil 'alpha)))
     (set-frame-parameter
      nil 'alpha
      (if (eql (cond ((numberp alpha) alpha)
                     ((numberp (cdr alpha)) (cdr alpha))
                     ;; Also handle undocumented (<active> <inactive>) form.
                     ((numberp (cadr alpha)) (cadr alpha)))
               100)
          '(85 . 50) '(100 . 100)))))
 (global-set-key (kbd "C-c t") 'toggle-transparency)

;;don't highlight the end of long lines
(setq whitespace-line-column 99999)

(setq hi-lock-file-patterns-policy #'(lambda (dummy) t)) 

(put 'dired-find-alternate-file 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
	
;; This snippet loads all *.el files in a directory.
(defun load-directory (dir)
  (let ((load-it (lambda (f)
                   (load-file (concat (file-name-as-directory dir) f)))
                 ))
    (mapc load-it (directory-files dir nil "\\.el$"))))
    (load-directory "~/.emacs.d/elpa/")

(setq line-number-display-limit nil)
(setq line-number-display-limit-width 2000000)

;;________________________________________________________________
;;    Icomplete Mode Setup
;;________________________________________________________________

;; WARNING: copy/rename file in DIRED OVERWRITES

;; with fido mode on, when copy or move a file in dired,
;; be sure to press [Ctrl+d] to select current dir.
;; Otherwise, if you press Enter, it selects current file choice and prompt to override file.

(if (version< emacs-version "28.1")
    (progn
      (progn
        ;; make buffer switch command do suggestions, also for find-file command
        (require 'ido)
        (ido-mode 1)
        ;; show choices vertically
        (setf (nth 2 ido-decorations) "\n")
        ;; show any name that has the chars you typed
        (setq ido-enable-flex-matching t)
        ;; use current pane for newly opened file
        (setq ido-default-file-method 'selected-window)
        ;; use current pane for newly switched buffer
        (setq ido-default-buffer-method 'selected-window)
        )
      (progn
        ;; minibuffer enhanced completion icomplete
        (require 'icomplete)
        (icomplete-mode 1)
        ;; show choices vertically
        (setq icomplete-separator "\n")
        (setq icomplete-hide-common-prefix nil)
        (setq icomplete-in-buffer t)
        (define-key icomplete-minibuffer-map (kbd "<right>") 'icomplete-forward-completions)
        (define-key icomplete-minibuffer-map (kbd "<left>") 'icomplete-backward-completions)))
  (fido-vertical-mode 1))
