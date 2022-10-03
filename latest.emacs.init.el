;; A dashboard on startup can clean my mind
(setq emacs-banner "~/pix/tom.png")
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (if (file-exists-p emacs-banner)
     (setq-default dashboard-startup-banner emacs-banner))

  (setq dashboard-items '((recents  . 5)
                          (bookmarks . 5)
;                          (projects . 5)
                          (agenda . 5))))

;; comment ON/OFF to DEBUG init
;; (setq debug-on-error t)

; overkill this as much as possible, never works anyway
(set-language-environment 'utf-8)
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;;________________________________________________________________
;;    ORG MODE
;;________________________________________________________________
;; Remove the emphasis markers from your Org Mode buffer
(setq org-hide-emphasis-markers t)
(setq org-latex-inputenc-alist '(("utf8" . "utf8x")))

(setq org-log-done 'time) ; I need to know when a task is done
;; Custom org block style
(defun teddy-ma/org-block-setup ()
  (let ((background-color (face-background 'default))
        (foreground-color (face-foreground 'default))
        (primary-green "#60a83d"))
    (set-face-attribute 'fringe nil :foreground foreground-color :background background-color)
    (set-face-attribute 'org-indent nil :background nil :foreground nil)
    (set-face-attribute 'org-block-begin-line nil :foreground primary-green :background nil)
    (set-face-attribute 'org-block-end-line nil   :foreground primary-green :background nil))
  )
;;(set-face-attribute 'org-block nil :background "red")))
;;(set-face-attribute 'org-level-2 nil :height 1.3 :background "#60a83d")

;;  Ellipsis
(setq org-hide-emphasis-markers nil)
(setq org-ellipsis " ⤵") ;; ↴, ▼, ▶, ⤵, ▾

;; Beautify Org Checkbox Symbol
(defun teddy-ma/org-buffer-setup ()
  (push '("[ ]" . "☐" ) prettify-symbols-alist)
  (push '("[X]" . "☑" ) prettify-symbols-alist)
  (push '("[-]" . "❍" ) prettify-symbols-alist)
  (prettify-symbols-mode)
  )
;; (setq-default prettify-symbols-alist '(("#+BEGIN_SRC" . "»") ("#+END_SRC" . "«")("#+begin_src" . "»") ("#+end_src" . "«") ("lambda"  . "λ") ("->" . "→")

;; Exported html should have no default style. I can style it myself:
(setq org-html-head-include-default-style nil
      org-html-htmlize-output-type 'css)

;; Execute Org src block
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (ruby . t)
   (js .t )
   (lua .t )
   (shell . t)
   (plantuml . t)))

;; Improve org mode looks
;; (setq org-startup-indented t
;;       org-pretty-entities t
;;       org-hide-emphasis-markers t
;;       org-startup-with-inline-images t
;;       org-image-actual-width '(300))

;; ;; Show hidden emphasis markers
;; (use-package org-appear
;;   :hook (org-mode . org-appear-mode))

;; ###----startup performance----###

;; make startup faster by reducing the frequency of garbage collection and then use a hook to measure Emacs startup time.
;; The default is 800 kilobytes.  Measured in bytes.
;; Garbage collection off during initialization (focus all memory on initialize)
(setq gc-cons-threshold (* 50 1000 1000)
      gc-cons-percentage 0.6)
;reset garbage collection after initialization (return deprecated memory to stack when idle)
(add-hook 'emacs-startup-hook
  (lambda ()
    (setq gc-cons-threshold  (* 2 1000 1000)
          gc-cons-percentage 0.1)))

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))
;;________________________________________________________________
;;    Auto Completion
;;________________________________________________________________
;; (require 'auto-complete)
;; (global-auto-complete-mode t)
;; (setq ac-modes '(sh-mode lisp-mode c-mode c++-mode sql-mode html-mode)) ; you can specified only for some certain mode

(setq powerline-default-separator nil)

(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("dired" (mode . dired-mode))
               ("org" (name . "^.*org$"))
               ("web" (or (mode . web-mode) (mode . js2-mode)))
               ("shell" (or (mode . eshell-mode) (mode . shell-mode)))
               ("mu4e" (name . "\*mu4e\*"))
               ("programming" (or
                               (mode . python-mode)
                               (mode . c++-mode)))
               ("emacs" (or
                         (name . "^\\*scratch\\*$")
                         (name . "^\\*Messages\\*$")))
               ))))

(add-hook 'ibuffer-mode-hook
(lambda ()
(ibuffer-auto-mode 1)
(ibuffer-switch-to-saved-filter-groups "default")))

;; don't show these
;(add-to-list 'ibuffer-never-show-predicates "zowie")
;; Don't show filter groups if there are no buffers in that group
(setq ibuffer-show-empty-filter-groups nil)

;; Don't ask for confirmation to delete marked buffers
(setq ibuffer-expert t)

;; Spell checking
;; Requires Hunspell
(use-package flyspell
  :config
  (setq ispell-program-name "hunspell"
        ispell-default-dictionary "en_GB")
;  :hook (text-mode . flyspell-mode)
  :bind (("M-<f7>" . flyspell-buffer)
         ("<f7>" . flyspell-word)
         ("C-;" . flyspell-auto-correct-previous-word)))

(use-package emmet-mode
:ensure t
:config
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'web-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode)) ;; enable Emmet's css abbreviation.

;; ──────────────────── Better interaction with X clipboard ────────────────────
(setq-default
 ;; Makes killing/yanking interact with the clipboard.
 x-select-enable-clipboard t

 ;; Save clipboard strings into kill ring before replacing them. When
 ;; one selects something in another program to paste it into Emacs, but
 ;; kills something in Emacs before actually pasting it, this selection
 ;; is gone unless this variable is non-nil.
 save-interprogram-paste-before-kill t

 ;; Shows all options when running apropos. For more info,
 ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Apropos.html.
 apropos-do-all t

 ;; Mouse yank commands yank at point instead of at click.
 mouse-yank-at-point t)
;;________________________________________________________________
;;    Global Key Bindings
;;________________________________________________________________
(add-hook 'emacs-lisp-mode-hook
		  (lambda ()
			(define-key emacs-lisp-mode-map (kbd "C-c C-b")
			  (lambda ()
				"Save and evaluate-buffer."
				(interactive)
				(save-buffer)
				(eval-buffer)))))

(define-key esc-map "&" 'query-replace-regexp)		; redefined ESC-&
(global-set-key (kbd "M-#") 'query-replace-regexp)
(global-set-key (kbd "M-\"") 'insert-pair)			; Wrap text in quotes
;(global-set-key (kbd "TAB") 'self-insert-command)	; To make sure that emacs is actually using TABS instead of SPACES

;; I use C-h for backspace in Emacs and move `help-command' elsewhere:
(global-set-key "\C-h" 'backward-delete-char-untabify)
;(define-key isearch-mode-map "\C-h" 'isearch-delete-char)
(global-set-key (kbd "C-S-H") 'kill-whole-line)
(global-set-key (kbd "<f12>") 'help-command)
(global-set-key "\C-c\C-c" 'comment-or-uncomment-region)
(global-set-key "\C-t" 'toggle-truncate-lines) ; this lets us have long lines go off the side of the screen instead of hosing up the ascii art
(global-set-key "\C-c\C-d" "\C-a\C- \C-n\M-w\C-y")	; Duplicate a whole line
(global-set-key (kbd "C-S-R") 'rename-file)
(global-set-key "\C-cD" 'Delete-current-file)
(global-set-key "\C-z" 'shell)
;; (global-set-key "\C-z" 'call-last-kbd-macro)		; call-last-kbd-macro frequently used key on a double key sequence (I think original is ^Xe)
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)
(global-set-key "\M-o"  'other-window)
(global-set-key "\M-n"  'next-buffer)
(global-set-key "\M-p"  'previous-buffer)
(global-set-key (kbd "C-.") #'other-window)
(global-set-key (kbd "C-,") #'prev-window)
(defun prev-window ()
  (interactive)
  (other-window -1))

;; spell check for Bangla text
(global-set-key (kbd "C-c B")
                (lambda()(interactive)
                  (ispell-change-dictionary "bn_BD")
                  (flyspell-buffer)))
;; Toggle show-trailing-whitespace.
(global-set-key (kbd "C-c M-w") (function (lambda () (interactive) (setq show-trailing-whitespace (not show-trailing-whitespace)))))

(windmove-default-keybindings)
(global-set-key (kbd "s-<left>")    'windmove-left)
(global-set-key (kbd "s-<right>")   'windmove-right)
(global-set-key (kbd "s-<down>")    'windmove-down)
(global-set-key (kbd "s-<up>")      'windmove-up)

(global-set-key (kbd "C-c <left>")    'windswap-left)
(global-set-key (kbd "C-c <right>")   'windswap-right)
(global-set-key (kbd "C-c <down>")    'windswap-down)
(global-set-key (kbd "C-c <up>")      'windswap-up)

(global-set-key (kbd "M-t") nil) ;; Remove the old keybinding
(global-set-key (kbd "M-t c") 'transpose-chars)
(global-set-key (kbd "M-t w") 'transpose-words)
(global-set-key (kbd "M-t t") 'transpose-words)
(global-set-key (kbd "M-t M-t") 'transpose-words)
(global-set-key (kbd "M-t l") 'transpose-lines)
(global-set-key (kbd "M-t e") 'transpose-sexps)
(global-set-key (kbd "M-t s") 'transpose-sentences)
(global-set-key (kbd "M-t p") 'transpose-paragraphs)

;; Remember and restore the last cursor location of opened files
(save-place-mode 1)
(setq save-place-file (locate-user-emacs-file "places" ".emacs-places"))

;; may be running afoul of save-place-forget-unreadable-files.
;; On exit, it checks that every loaded file is readable before
;; saving its buffer position. potentially very slow if you use NFS.
;; bellow cmnd will restores emacs exit to nearly instantaneous.
;; (setq save-place-forget-unreadable-files nil)

(global-auto-revert-mode 1)							; Automatically update buffers if file content on the disk has changed.
(setq global-auto-revert-non-file-buffers t)		; Revert Dired and other buffers
;;________________________________________________________________
;;    Separte Customization from init file
;;________________________________________________________________
(setq custom-file (locate-user-emacs-file "custom.el")) ; Move customization variables to a separate file ~/.emacs.d/custom.el and load it
(unless (file-exists-p custom-file)
  (with-temp-buffer
    (write-file custom-file)))
(load custom-file 'noerror 'nomessage)
;;________________________________________________________________
;;    Modeline
;;________________________________________________________________

;; Basic Customization
(setq display-time-format "%l:%M%P (%a) %e %b ♪" 	; %D for date format
      display-time-default-load-average nil)

(setq line-move-visual t)
;; (setq mode-line-compact t)

;; Permanent display of line and column numbers is handy.
(size-indication-mode 1)
(display-time-mode)
(display-battery-mode)

;; Sensible line breaking
(add-hook 'text-mode-hook 'visual-line-mode)
;; Scroll to the first and last line of the buffer
(setq scroll-error-top-bottom t)
;;________________________________________________________________
;;    Balancing Parentheses
;;________________________________________________________________
(electric-pair-mode 1)			    ; auto close bracket insertion
;; make electric-pair-mode work on more brackets
(setq electric-pair-pairs
      '(
        (?\" . ?\")
        (?\< . ?\>)
        (?\{ . ?\})))
;;________________________________________________________________
;;    Editing Related
;;________________________________________________________________
(delete-selection-mode t)		; By default emacs will not delete selection text when typing on it, let's fix it
(setq kill-whole-line t) 			; kills the entire line plus the newline whenever you invoke kill-line (i.e. via C-k).

;; ###----Setting Key Bindings with use-package----###

(ffap-bindings)		; find-file-at-point, smarter C-x C-f when point on path or URL

;; Saves the minibuffer history on every Emacs session.
(savehist-mode t)
(setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
(setq history-length 25)

;; To set the file which the minibuffer is saved use:
(setq savehist-file "~/.emacs.d/tmp/savehist")

(when window-system (global-prettify-symbols-mode t))

;; Scratch buffer
;; (setq inhibit-startup-message t
;;       initial-scratch-message "#+title: Scratch Buffer\n\nWelcome to Emacs for content creators.\n\nPlease change the folder locations and fonts in the configuration file to match your preferences.\n\nGo to the [[https://lucidmanager.org/productivity/configure-emacs/][Lucid Manager]] website for full documentation of this configuration file.\n"
;;       initial-major-mode 'org-mode)
(defalias 'yes-or-no-p 'y-or-n-p)		; Ask y or n instead of yes or no

;; Open dired in same buffer
;; Sort Dired buffers
(setq dired-listing-switches "-agho --group-directories-first")
;; Copy and move files netween dired buffers
(setq dired-dwim-target t)
;; Define external image viewer/editor
(setq image-dired-external-viewer "/usr/bin/sxiv") ;or /usr/bin/gimp
;; Image-dired Keyboard shortcuts
(with-eval-after-load 'dired
    (define-key dired-mode-map (kbd "C-c C-i") 'image-dired)
    (define-key dired-mode-map (kbd "M-<return>") 'image-dired-dired-display-external))

;; Disable some default feature
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; (load-theme 'tango-dark t)
;; (set-foreground-color "ivory")
;; (set-background-color "darkblue")
;; (set-face-attribute 'region nil :background "#666" :foreground "#ffffff")	; Change the HIGHLIGHT COLOR for SELECTED TEXT
;; (set-face-attribute 'highlight nil :foreground 'unspecified)

(setq display-line-numbers 'relative)
(setq display-line-numbers-width 0)
(set-frame-parameter nil 'fullscreen 'fullheight)
;;________________________________________________________________
;;    Fonts Setting
;;________________________________________________________________
(global-font-lock-mode 1)			; Use font-lock everywhere.
(setq font-lock-maximum-decoration t)		; We have CPU to spare; highlight all syntax categories.

(setq default-input-method "bengali-probhat")
(set-fontset-font "fontset-default" 'bengali (font-spec :family "Kalpurush" :size 14))

(defun remove-quail-show-guidance ()
  nil)
(defun remove-quail-completion ()
  (quail-select-current))
(defun bn-company-wordfreq ()
  (interactive)
  (advice-add 'quail-show-guidance :override #'remove-quail-show-guidance)
  (advice-add 'quail-completion :override #'remove-quail-completion)
  (setq ispell-local-dictionary "bengali")
  (setq-local company-backends '(company-wordfreq))
  (setq-local company-transformers nil))
(add-hook 'text-mode-hook (lambda ()
                            (setq-local company-backends '(company-wordfreq))
                            (setq-local company-transformers nil)))

(set-face-attribute 'default nil
		            :font "Fantasque Sans Mono" ; "JetBrains Mono"
		            :weight 'light
		            :height (cond ((string-equal system-type "gnu/linux") 110)
				                  ((string-equal system-type "darwin") 130)))
;; (set-frame-font "Comic Mono-10.5" nil t)
;; (set-frame-font "Monaco-9" nil t)
;; (set-frame-font "Fantasque Sans Mono-10.5" nil t)
;; (set-frame-font "Source Code Pro-10" nil t)
;; (set-frame-font "Fira Code-10" nil t)

;;________________________________________________________________
;;    Cursor Mode
;;________________________________________________________________
(set-mouse-color "white")
(setq x-stretch-cursor t)		; make cursor the width of the character it is under i.e. full width of a TAB
(defun djcb-set-cursor-according-to-mode ()
    "change cursor color and type according to some minor modes."
    (cond
     (buffer-read-only
      (set-cursor-color "yellow")
      (setq cursor-type '(hbar . 3)))
     (overwrite-mode
      (set-cursor-color "red")
      (setq cursor-type 'hollow))
     (t
;;      (set-cursor-color "lightblue")
      (set-cursor-color "DeepSkyBlue")
      (setq cursor-type '(bar . 2)))))
(add-hook 'post-command-hook 'djcb-set-cursor-according-to-mode)
(blink-cursor-mode 1)			; Turn-off Cursor Blink (1 to Enable & 0 to Stop)

(defun ljos/back-to-indentation|beginning-of-line ()
  "Moves the cursor back to indentation or to the beginning of
the line if it is already at the indentation.  If it is at the
beginning of the line it stays there."
  (interactive)
  (when (not (bolp))
    (let ((p (point)))
      (back-to-indentation)
      (when (= p (point))
        (beginning-of-line 1)))))

(global-set-key (kbd "C-a") #'ljos/back-to-indentation|beginning-of-line)
;;________________________________________________________________
;;    Backup Files
;;________________________________________________________________
(defvar --backup-directory (concat user-emacs-directory "backups"))
(if (not (file-exists-p --backup-directory))
        (make-directory --backup-directory t))

(setq-default backup-by-copying t               ; don't clobber symlinks
              backup-directory-alist `(("." . ,--backup-directory))
              default-directory "~/"
              load-prefer-newer t 				; don't use the compiled code if its the older package.
              make-backup-files t               ; backup of a file the first time it is saved.
			  version-control t                 ; version numbers for backup files
			  delete-by-moving-to-trash t		; move deleted files to trash
			  delete-old-versions t             ; delete excess backup files silently
			  kept-old-versions 2               ; oldest versions to keep when a new numbered backup is made (default: 2)
			  kept-new-versions 6               ; newest versions to keep when a new numbered backup is made (default: 2)
			  auto-save-default t               ; auto-save every buffer that visits a file
			  auto-save-timeout 30              ; number of seconds idle time before auto-save (default: 30)
			  auto-save-interval 200            ; number of keystrokes between auto-saves (default: 300)
              show-paren-delay 0           		; how long to wait?
              show-paren-style 'mixed      		; alternatives are 'expression' and 'parenthesis'
              enable-recursive-minibuffers t    ; allow commands to be run on minibuffers.
              scroll-conservatively 10
			  scroll-margin 1
              scroll-preserve-screen-position t
              inhibit-startup-buffer-menu t
              inhibit-startup-screen t 			; Do not show the startup message.
              visible-bell t					; Flash the screen on error, don't beep.
              view-read-only t					; Toggle ON or OFF with M-x view-mode (or use e to exit view-mode).
              use-dialog-box nil				; Don't pop up UI dialogs when prompting
              tab-width 4
              indent-tabs-mode nil			; set indentation with spaces instead of tabs with 4 spaces
              indent-line-function 'insert-tab
              backward-delete-char-untabify-method 'hungry ; Alternatives is: 'all (remove all consecutive whitespace characters, even newlines)
              require-final-newline t
              save-interprogram-paste-before-kill t
              message-log-max 1000
              fill-column 80
              column-number-mode t               ; Show (line,column) in mode-line
              cua-selection-mode t ; Delete regions
              ;; Use your name in the frame title. :)
              frame-title-format (format "%s's Emacs" (if (or (equal user-login-name "suvratapte")
                                                              (equal user-login-name "suvrat.apte"))
                                                          "Suvrat"
                                                        (capitalize user-login-name)))
              )
(setq delete-auto-save-files t)		; deletes buffer's auto save file when it is saved or killed with no changes in it.
;; ─────────────────── Added functionality (Generic usecases) ──────────────────
;; Unfill paragraph
;; Might be good. For instance for canceling all of the paragraph quickly or for commenting it away.
(defun unfill-paragraph ()
  "Convert a multi-line paragraph into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
	(fill-paragraph nil)))
;; Handy key definition
(define-key global-map "\M-Q" 'unfill-paragraph)


(defun comment-pretty ()
  "Insert a comment with '─' (C-x 8 RET BOX DRAWINGS LIGHT HORIZONTAL) on each side."
  (interactive)
  (let* ((comment-char "─")
         (comment (read-from-minibuffer "Comment: "))
         (comment-length (length comment))
         (current-column-pos (current-column))
         (space-on-each-side (/ (- fill-column
                                   current-column-pos
                                   comment-length
                                   (length comment-start)
                                   ;; Single space on each side of comment
                                   (if (> comment-length 0) 2 0)
                                   ;; Single space after comment syntax sting
                                   1)
                                2)))
    (if (< space-on-each-side 2)
        (message "Comment string is too big to fit in one line")
      (progn
        (insert comment-start)
        (when (equal comment-start ";")
          (insert comment-start))
        (insert " ")
        (dotimes (_ space-on-each-side) (insert comment-char))
        (when (> comment-length 0) (insert " "))
        (insert comment)
        (when (> comment-length 0) (insert " "))
        (dotimes (_ (if (= (% comment-length 2) 0)
                      (- space-on-each-side 1)
                      space-on-each-side))
          (insert comment-char))))))

(global-set-key (kbd "C-c ;") 'comment-pretty)

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
;;		Identity Who I Am ?
;;________________________________________________________________

;; Do not publish my email on Usenet
(setq user-full-name       "Likhon Barai"
      user-login-name      "Likhon-art"
      user-real-login-name "Likhon"
      user-mail-address    "likhonhere007@gmail.com"
      system-name          "Likhon.Art")
;;________________________________________________________________
;;		Highlight Current LINE
;;________________________________________________________________
(when window-system (global-hl-line-mode 1))
(set-face-background 'highlight "#3e4446")	; you canalso try: "#3e4446" or "#gray6" etc.
(set-face-foreground 'highlight nil)
;; (set-face-underline-p 'highlight "#ff0000")

;; (when window-system (vline-global-mode 1))
;; (set-face-background 'vline "#3e4446")	; you canalso try: "#ff0000" or "#gray6" or etc.
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

;; ────────────────────────────── Generic packages ─────────────────────────────
(require 'package)
(setq package-enable-at-startup nil) ; dont do it immediately
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")))
;; Install use-package if not installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents) ; update archives
  (package-install 'use-package)) ; grab the newest use-package

(eval-and-compile
  (setq use-package-always-ensure t)
  (setq use-package-expand-minimally t)
  (setq use-package-compute-statistics t)
  (setq use-package-enable-imenu-support t))

(eval-when-compile
  (require 'use-package)
  (require 'bind-key))

;; ──────────────── Additional packages and their configurations ───────────────

;; Add `:doc' support for use-package so that we can use it like what a doc-strings is for
;; functions.
(eval-and-compile
  (add-to-list 'use-package-keywords :doc t)
  (defun use-package-handler/:doc (name-symbol _keyword _docstring rest state)
    "An identity handler for :doc.
     Currently, the value for this keyword is being ignored.
     This is done just to pass the compilation when :doc is included
     Argument NAME-SYMBOL is the first argument to `use-package' in a declaration.
     Argument KEYWORD here is simply :doc.
     Argument DOCSTRING is the value supplied for :doc keyword.
     Argument REST is the list of rest of the  keywords.
     Argument STATE is maintained by `use-package' as it processes symbols."

    ;; just process the next keywords
    (use-package-process-keywords name-symbol rest state)))

(use-package auto-package-update
  :if (not (daemonp))
  :custom
  (auto-package-update-interval 7) ;; in days
  (auto-package-update-prompt-before-update t)
  (auto-package-update-delete-old-versions t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe))

(use-package diminish) ; Diminish, a feature that removes certain minor modes from mode-line.

(use-package delight
  :ensure t
  :delight)

;; Benchmark startup
;; benchmark-init records startup time by package so we can debug. It only records things after it’s initialised, so put as early in config as possible.
(use-package benchmark-init
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(add-hook 'after-init-hook
          (lambda () (message "loaded in %s" (emacs-init-time))))

(use-package uniquify-files
  :config
  (setq uniquify-buffer-name-style 'reverse)
  (setq uniquify-separator " • ")
  (setq uniquify-after-kill-buffer-p t)
  (setq uniquify-ignore-buffers-re "^\\*"))

;; Show recent files in the File menu.
(use-package recentf
  :doc "Recent buffers in a new Emacs session"
  :config
  (setq recentf-auto-cleanup 'never
        recentf-max-saved-items 500
        recentf-max-menu-items 60
        recentf-save-file (concat user-emacs-directory ".recentf"))
  (recentf-mode t)
  :delight)

(use-package magit
  :doc "Git integration for Emacs"
  :ensure t
  :bind ("C-x g" . magit-status)
  :delight)

(use-package ibuffer
  :doc "Better buffer management"
  :bind ("C-x C-b" . ibuffer)
  :delight)

(use-package all-the-icons
  :if (display-graphic-p))
;;  :config (all-the-icons-install-fonts 'install-without-asking))
;; (cl-defun all-the-icons-faicon (icon &rest _)
;;  #("" 0 1 (rear-nonsticky t display (raise -0.24) font-lock-face (:family "FontAwesome" :height 1.2) face (:family "FontAwesome" :height 1.2)))))


;; Use all the icons for dired
;; (use-package all-the-icons-dired
;;   :quelpa (shell-switcher :fetcher github :repo "jtbm37/all-the-icons-dired.git")
;;   :ensure t
;;   )
;; (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

(use-package which-key
  :ensure t
  :init
  (which-key-mode)
  (setq which-key-sort-order 'which-key-key-order-alpha
        which-key-idle 0.5
        which-key-idle-dely 50)
  (which-key-setup-minibuffer))
;  (which-key-setup-side-window-right)

;; Goto last change
;; Sometimes it's useful to step to the last changes in a buffer.
(use-package goto-last-change
  :bind (("C-;" . goto-last-change)))

;; Allow tree-semantics for undo operations.
(use-package undo-tree
  :diminish                       ;; Don't show an icon in the modeline
  :bind ("C-x u" . undo-tree-visualize)
  :hook (org-mode . undo-tree-mode) ;; For some reason, I need this. FIXME.
  :config
    ;; Always have it on
    (global-undo-tree-mode)

    ;; Each node in the undo tree should have a timestamp.
    (setq undo-tree-visualizer-timestamps t)

    ;; Show a diff window displaying changes between undo nodes.
    (setq undo-tree-visualizer-diff t))

(use-package winner
  :doc "a minor mode that records your window configurations and lets you undo and redo changes made to it."
  :config
  (winner-mode 1)
  :bind (("M-[" . winner-undo)
         ("M-]" . winner-redo)))

(use-package aggressive-indent
  :doc "Intended Indentation"
  :ensure t
  :config
  ;; (add-hook 'before-save-hook 'aggressive-indent-indent-defun)
  ;; Have a way to save without indentation.
  ;; (defun save-without-aggresive-indentation ()
  ;;   (interactive)
  ;;   (remove-hook 'before-save-hook 'aggressive-indent-indent-defun)
  ;;   (save-buffer)
  ;;   (add-hook 'before-save-hook 'aggressive-indent-indent-defun))
  ;; :bind (("C-x s" . save-without-aggresive-indentation))
  :delight)

(use-package beacon
  :ensure t
  :init
  (beacon-mode t)
  (setq beacon-color "#50D050") ;; a light green
  )

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package doom-themes
  :config
  ;; Global settings (default)
  (setq doom-themes-enable-bold t 	 ; if nil, bold is universally disabled
        doom-themes-enable-italic t  ; if nil, italics is universally disabled
        doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (load-theme 'doom-gruvbox t)
  (doom-themes-visual-bell-config)	 ; Enable flashing mode-line on errors
  (doom-themes-neotree-config)	 ; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-org-config)		  ; Corrects (and improves) org-mode's native fontification.
  )
;; doom-one has no italics. I added few with
(set-face-attribute 'font-lock-comment-face nil  :family "Comic Mono" :slant 'italic :height 100)
(set-face-attribute 'font-lock-function-name-face nil :slant 'italic :weight 'bold)
(set-face-attribute 'font-lock-variable-name-face nil :weight 'bold)
(set-face-attribute 'font-lock-keyword-face nil :weight 'bold)

;; (set-face-attribute 'font-lock-comment-face nil :foreground "#5B6268" :slant 'italic)
;; (set-face-attribute 'font-lock-function-name-face nil :foreground "#c678dd" :slant 'italic :weight 'bold)
;; (set-face-attribute 'font-lock-variable-name-face nil :foreground "#dcaeea" :weight 'bold)

(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1)
  (setq doom-modeline-lsp t
         ;; doom-modeline-height 1
         ;; doom-modeline-bar-width 3
        doom-modeline-project-detection 'project))

(use-package ivy
  :doc "A generic completion mechanism"
  :ensure t
  :config
  (ivy-mode t)
  (setq ivy-use-virtual-buffers t
        ;; Display index and count both.
        ivy-count-format "(%d/%d) "
        ;; By default, all ivy prompts start with `^'. Disable that.
        ivy-initial-inputs-alist nil)

  :bind (("C-x b" . ivy-switch-buffer)
         ("C-x B" . ivy-switch-buffer-other-window)
         ("C-c C-r" . ivy-resume)
         ("<f6>" . ivy-resume)
         ("C-c v" . ivy-push-view)
  		 ("C-c V" . ivy-pop-view))
  :delight)

(use-package ivy-rich
  :doc "Have additional information in empty space of ivy buffers."
  :disabled t
  :ensure t
  :custom
  (ivy-rich-path-style 'abbreviate)
  :config
  (setcdr (assq t ivy-format-functions-alist)
          #'ivy-format-function-line)
  (ivy-rich-mode 1)
  :delight)

(use-package ivy-posframe
  :doc "Custom positions for ivy buffers."
  :ensure t
  :config
  (when (member "Hasklig" (font-family-list))
    (setq ivy-posframe-parameters
          '((font . "Hasklig"))))

  (setq ivy-posframe-border-width 10)

  (setq ivy-posframe-display-functions-alist
        '((complete-symbol . ivy-posframe-display-at-point)
          (swiper . ivy-display-function-fallback)
          (swiper-isearch . ivy-display-function-fallback)
          (counsel-rg . ivy-display-function-fallback)
          (t . ivy-posframe-display-at-frame-center)))

  (ivy-posframe-mode t)
  :delight)

;; Prescient sorts and filters candidate lists for avy/counsel.
(use-package prescient)
(use-package ivy-prescient
  :config
  (ivy-prescient-mode t))

(use-package swiper
  :doc "A better search"
  :ensure t
  :bind (("C-s" . swiper)) ; ("C-s" . swiper-isearch))
  :delight)

;; it looks like counsel is a requirement for swiper
(use-package counsel
  :ensure t
  :bind (("M-x" . counsel-M-x)
         ("M-y" . counsel-yank-pop)
         ("C-x b" . counsel-switch-buffer)
         ("C-c c" . counsel-compile)
         ("C-c d" . counsel-dired)
         ("C-c F" . counsel-org-file)
         ("C-c g" . counsel-git)
         ("C-c i" . counsel-imenu)
         ("C-c j" . counsel-git-grep)
         ("C-c f" . counsel-file-jump)
         ("C-x l" . counsel-locate)
         ("C-c L" . counsel-git-log)
         ("C-c m" . counsel-linux-app)
         ("C-c n" . counsel-fzf)
         ("C-c o" . counsel-outline)
         ("C-c p" . counsel-package)
         ("C-c T" . counsel-load-theme)
         ("C-c z" . counsel-bookmark)
         ("C-x C-r" . counsel-recentf)
         ("C-x C-f" . counsel-find-file)
		 ("<f1> f" . counsel-describe-function)
		 ("<f1> v" . counsel-describe-variable)
		 ("<f1> l" . counsel-load-library)
		 ("<f1> L" . counsel-find-library)
		 ("<f2> i" . counsel-info-lookup-symbol)
		 ("<f2> j" . counsel-set-variable)
		 ("<f2> u" . counsel-unicode-char)
		 ; ("C-c /" . counsel-ag)
         ; ("C-c s" . counsel-rg)
		 ; ("C-S-o" . counsel-rhythmbox)
         ; (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
		 :map counsel-find-file-map
         ("RET" . ivy-alt-done))
  :delight)

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package company
  :ensure t
  :diminish t
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  (add-hook 'elm-mode-hook 'company-mode)
  :config
  (setq company-show-numbers t
        company-idle-delay 0.02
        company-minimum-prefix-length 2
        company-echo-delay 0
        company-dabbrev-downcase nil
        company-selection-wrap-around t
        company-dabbrev-code-everywhere t
        company-dabbrev-code-modes t
        company-dabbrev-code-ignore-case t
        company-tooltip-align-annotations t
        company-transformers '(company-sort-prefer-same-case-prefix)))
(global-set-key (kbd "<tab>") #'company-indent-or-complete-common)
(global-set-key (kbd "M-TAB") #'company-complete)
; Use tab key to cycle through suggestions.
; ('tng' means 'tab and go')
(company-tng-configure-default)
(setq company-format-margin-function #'company-vscode-light-icons-margin)
(with-eval-after-load 'company
;  (define-key company-active-map (kbd "M-/") #'company-complete)
  (define-key company-active-map (kbd "M-.") #'company-show-location)
  (define-key company-active-map (kbd "RET") nil)
  (define-key company-active-map
              (kbd "<backtab>")
              (lambda ()
                (interactive)
                (company-complete-common-or-cycle -1))))

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :config
  (yas-global-mode t)
  (add-to-list 'hippie-expand-try-functions-list
               'yas-hippie-try-expand)
  (global-set-key (kbd "M-/") 'company-yasnippet))

(use-package yasnippet-snippets
  :ensure t)

;; Required for proportional font
(use-package company-posframe
  :config
  (company-posframe-mode t))

(use-package company-wordfreq
  :ensure t)

(use-package avy
  :ensure t)
(global-set-key (kbd "C-'") 'avy-goto-char)
(global-set-key (kbd "C-:") 'avy-goto-char-2)
(global-set-key (kbd "M-g f") 'avy-goto-line)
(global-set-key (kbd "M-g w") 'avy-goto-word-1)
(global-set-key (kbd "M-g e") 'avy-goto-word-0)
(global-set-key (kbd "C-c C-j") 'avy-resume)

;; Nyan Cat is lovely, it can live on mode line
(use-package nyan-mode
  :ensure t
  :init
  (setq nyan-animate-nyancat t)
  (setq nyan-wavy-trail t)
  (setq nyan-minimum-window-width 80)
  (setq nyan-bar-length 75)
  (nyan-mode))

;; never want whitespace at the end of lines. Remove it on save.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Browse source tree with Speedbar file browser
(setq speedbar-show-unknown-files t)
(setq company-backends (delete 'company-semantic company-backends))

;; This snippet loads all *.el files in a directory.
(defun load-directory (dir)
  (let ((load-it (lambda (f)
                   (load-file (concat (file-name-as-directory dir) f)))
                 ))
    (mapc load-it (directory-files dir nil "\\.el$"))))
    (load-directory "~/.emacs.d/elpa/")

(setq line-number-display-limit nil)
(setq line-number-display-limit-width 2000000)

(setq mail-user-agent 'message-user-agent)
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-stream-type 'starttls
      smtpmail-smtp-server "mail.domain.com"
      smtpmail-smtp-service 587)

;; Open File with Line Number
;; Open files and goto lines like we see from g++ etc. i.e. Gemfile:12
(defadvice find-file (around find-file-line-number
                             (filename &optional wildcards)
                             activate)
  "Turn files like file.cpp:14 into file.cpp and going to the 14-th line."
  (save-match-data
    (let* ((matched (string-match "^\\(.*\\):\\([0-9]+\\):?$" filename))
           (line-number (and matched
                             (match-string 2 filename)
                             (string-to-number (match-string 2 filename))))
           (filename (if matched (match-string 1 filename) filename)))
      ad-do-it
      (when line-number
        ;; goto-line is for interactive use
        (goto-char (point-min))
        (forward-line (1- line-number))))))

(provide 'init)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
(put 'scroll-left 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;; ─────────────────────────────────── *ORG* ───────────────────────────────────
;; (load-file "~/.emacs.d/org-config.el")

;; ;; Open agenda view when Emacs is started.
;; ;; Do it only if it's Suvrat's computer.
;; (when (equal user-login-name "suvratapte")
;;   (jump-to-org-agenda)
;;   (delete-other-windows))

;; (provide 'init)
(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."
  (require 'xclip)
  (xclip-mode 1)

  (defvar company-mode/enable-yas t "Enable yasnippet for all backends.")

  (defun company-mode/backend-with-yas (backend)
    (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))

  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))

  (use-package lsp-mode
    :ensure t
    :commands lsp
    :hook (elm-mode . lsp)
    :config
    (setq lsp-prefer-flymake nil
          lsp-enable-symbol-highlighting nil
          lsp-enable-snippet nil
          lsp-ui-sideline-enable nil
          lsp-ui-flycheck-enable t))

  (use-package lsp-ui
    :ensure t
    :commands lsp-ui-mode)

  (use-package company-lsp
    :ensure t
    :commands company-lsp
    :config
    (push '(company-lsp :with company-yasnippet) company-backends)
    )

  (use-package helm-lsp :commands helm-lsp-workspace-symbol)
  (use-package lsp-treemacs :commands lsp-treemacs-errors-list)
  (use-package dap-mode)

  (add-hook 'elm-mode-hook (lambda () (defalias 'company-elm 'company-lsp)))
  )
