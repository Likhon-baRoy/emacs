;; Don't pop up UI dialogs when prompting
(setq use-dialog-box nil)

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

;; To make sure that emacs is actually using tabs instead of spaces:
(global-set-key (kbd "TAB") 'self-insert-command)

;; Duplicate a whole line
(global-set-key "\C-c\C-d" "\C-a\C- \C-n\M-w\C-y")

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
;; 
;; (defun my-c-mode-common-hook ()
;;   "my customizations for all of c-mode and related modes"
;;   (setq c-basic-offset 4)
;;   (setq c-basic-indent 4))
;; 
;; (add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
;;
;; Space / Tabs - Indentation

;; Set indentation with spaces instead of tabs with 4 spaces
(setq tab-width 4 indent-tabs-mode nil)

;; set default tab char's display width to 4 spaces
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;; make tab key always call a indent command
;; (setq-default tab-always-indent t)

;; force emacs to always use spaces instead of tab characters
(setq-default tab-always-indent nil)

;; make tab key call indent command or insert tab character, depending on cursor position
(setq-default tab-always-indent nil)

;; make tab key do indent first then completion.
(setq-default tab-always-indent 'complete)

(setq sentence-end-double-space nil )

;;find-file-at-point, smarter C-x C-f when point on path or URL
(ffap-bindings)

;; Unique buffer name
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Show recent files in the File menu.

;; recentf stuff
;; recentf - F5
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

;; Turn-off Startup en
(setq inhibit-startup-screen t)
;; Display this instead of "For information about GNU Emacs and the
;; GNU system, type C-h C-a.". This has been made intentionally hard
;; to customize in GNU Emacs so I have to resort to hackery.
(defun display-startup-echo-area-message ()
  "If it wasn't for this you'd be GNU/Spammed by now"
  (message ""))

;; Don't insert instructions in the *scratch* buffer
(setq initial-scratch-message nil)

;; Ask y or n instead of yes or no
(defalias 'yes-or-no-p 'y-or-n-p)

;; change all prompts to y or n
;; (fset 'yes-or-no-p 'y-or-n-p)

;; The built-in view-mode turns the current buffer into a read-only pager.
(setq-default view-read-only t)		 ; Toggle on or off with M-x view-mode (or use e to exit view-mode).

(setq-default display-line-numbers 'relative)
(setq-default display-line-numbers-width 2)

(load-theme 'tango-dark t)

;; I change the highlight color for selected text
(set-face-attribute 'region nil :background "#666" :foreground "#ffffff") 
;; (set-face-attribute 'highlight nil :foreground 'unspecified)

;; Disable some default feature
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(set-frame-parameter nil 'fullscreen 'fullheight)

;;________________________________________________________________
;;    Fonts Setting
;;________________________________________________________________

;; Use font-lock everywhere.
(global-font-lock-mode 1)

;; We have CPU to spare; highlight all syntax categories.
(setq font-lock-maximum-decoration t)

(set-frame-font "Comic Mono-10.5" nil t)
;; (set-frame-font "Monaco-9" nil t)
;; (set-frame-font "Fantasque Sans Mono-10.5" nil t)
;; (set-frame-font "Source Code Pro-10" nil t)
;; (set-frame-font "Fira Code-10" nil t)

;; It is much more pleasant and less tiring to use a dark background.
;; (set-foreground-color "ivory")
;; (set-background-color "darkblue")

;; Set cursor and mouse colours:
(set-cursor-color "Orchid3")
(set-mouse-color "white")

;(setq-default cursor-type '(bar . 2))

;; Enable Blink Cursor
(blink-cursor-mode 1)

;; Stop Blink Cursor
;;(blink-cursor-mode 0)

;; Turn-off Cursor Blink
;; (blink-cursor-mode -1)

;; make cursor the width of the character it is under
;; i.e. full width of a TAB
(setq x-stretch-cursor t)

;; prevent down-arrow from adding empty lines to the bottom of the buffer
;; (which is the default behaviour)
;; (setq next-line-add-newlines nil)

;;________________________________________________________________
;;    Screen Bell
;;________________________________________________________________

;; Turn-off Alarm Bell
;; (setq ring-bell-function #'ignore)

;; Flash the screen on error, don't beep.
(setq-default visible-bell t)

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

;; deletes buffer's auto save file when it is saved or killed with no changes in it.

(setq delete-auto-save-files t)

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
;; 

;; (global-hl-line-mode 1)
;; (set-face-background 'highlight nil)	; you canalso try: "#3e4446" or "#gray6" or etc.
;; (set-face-foreground 'highlight nil)
;; (set-face-underline-p 'highlight "#ff0000")
;; 
;; (vline-global-mode 1)
;; (set-face-background 'vline "#ff0000")	; you canalso try: "#3e4446" or "#gray6" or etc.
;; (set-face-foreground 'vline nil)
;; (setq vline-style 'mixed)

;;________________________________________________________________
;;    Transparent Emacs
;;________________________________________________________________

;; (set-frame-parameter (selected-frame) 'alpha '(<active> . <inactive>))
;; (set-frame-parameter (selected-frame) 'alpha <both>)
(set-frame-parameter (selected-frame) 'alpha '(85 . 50))
(add-to-list 'default-frame-alist '(alpha . (85 . 50)))

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
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

	
;; This snippet loads all *.el files in a directory.

;;     (defun load-directory (dir)
;;       (let ((load-it (lambda (f)
;; 		       (load-file (concat (file-name-as-directory dir) f)))
;; 		     ))
;; 	(mapc load-it (directory-files dir nil "\\.el$"))))
;;     (load-directory "~/.emacs.d/elpa/")

;; call-last-kbd-macro frequently used key on a double key sequence (I think original is ^Xe). I've also redefined ESC-&.
;; (global-set-key "\C-z" 'call-last-kbd-macro)
;; (define-key esc-map "&" 'query-replace-regexp)

(defvar my-mode-line-total-lines
  '("%l/" (:eval (number-to-string (line-number-at-pos (point-max)))))
  "Mode line snippet for current and total number of lines in buffer.")
(put 'my-mode-line-total-lines 'risky-local-variable t)

(add-to-list 'mode-line-format 'my-mode-line-total-lines t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mode-line-position-column-line-format '(" (%l,%c)"))
 '(package-selected-packages '(vline undo-tree bind-key)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(setq line-number-display-limit nil)
(setq line-number-display-limit-width 2000000)

;;** EMMS
 ;; Autoload the id3-browser and bind it to F7.
 ;; You can change this to your favorite EMMS interface.
 (autoload 'emms-smart-browse "emms-browser.el" "Browse with EMMS" t)
 (global-set-key [(f7)] 'emms-smart-browse)

 (with-eval-after-load 'emms
   (emms-standard) ;; or (emms-devel) if you want all features
   (setq emms-source-file-default-directory "~/musix"
         emms-info-asynchronously t
         emms-show-format "♪ %s")

   ;; Might want to check `emms-info-functions',
   ;; `emms-info-libtag-program-name',
   ;; `emms-source-file-directory-tree-function'
   ;; as well.

   ;; Determine which player to use.
   ;; If you don't have strong preferences or don't have
   ;; exotic files from the past (wma) `emms-default-players`
   ;; is probably all you need.
   
   (if (executable-find "mpv")
       (setq emms-player-list '(emms-player-mpv))
     (emms-default-players))
   
   (if (executable-find "mplayer")
       (setq emms-player-list '(emms-player-mplayer))
     (emms-default-players))

;;   ;; For libre.fm see `emms-librefm-scrobbler-username' and
   ;; `emms-librefm-scrobbler-password'.
   ;; Future versions will use .authoinfo.gpg.
   )
