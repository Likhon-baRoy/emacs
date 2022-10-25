;;; .emacs --- Initialization file for Emacs

;; Author: Likhon
;; Maintainer: Likhon
;; Version: version
;; Package-Requires: (dependencies)
;; Homepage: homepage
;; Keywords: keywords

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:

;;; Code:
;; Emacs as External Editor

;; Config Edit/Re-load
;; First Edit
(defun config-visit ()
  "Uncle dev created a function to find Emacs config."
  (interactive)
  (find-file "~/.emacs"))
(global-set-key (kbd "C-c e") 'config-visit)

(defun config-reload ()
  "Uncle dev created a function to reload Emacs config."
  (interactive)
  (load-file (expand-file-name "~/.emacs")))
(global-set-key (kbd "C-c r") 'config-reload)

;; A dashboard on startup can clean my mind
(use-package dashboard
  :diminish (dashboard-mode page-break-lines-mode)
  :config
  (setq dashboard-footer "Time teaches all things.")
  (setq dashboard-footer-icon (all-the-icons-octicon "calendar"
                                                     :height 1.1
                                                     :v-adjust -0.05
                                                     :face 'font-lock-keyword-face))

  (setq dashboard-navigator-buttons
        `(;; line1
          ((,(all-the-icons-octicon "mark-github" :height 1.1 :v-adjust 0.0)
            "Homepage"
            "Browse homepage"
            (lambda (&rest _) (browse-url "homepage")))
           ("★" "Star" "Show stars" (lambda (&rest _) (show-stars)) warning)
           ("?" "" "?/h" #'show-help nil "<" ">"))
          ;; line 2
          ((,(all-the-icons-faicon "linkedin" :height 1.1 :v-adjust 0.0)
            "Linkedin"
            ""
            (lambda (&rest _) (browse-url "homepage")))
           ("⚑" nil "Show flags" (lambda (&rest _) (message "flag")) error))))
  (setq dashboard-projects-backend 'project-el
        dashboard-startup-banner (concat user-emacs-directory "logos/emacs.png")
        dashboard-image-banner-max-height 160
        dashboard-banner-logo-title "Ποσειδον 🔱 εδιτορ"
        dashboard-set-heading-icons t
        dashboard-set-file-icons t
        dashboard-show-shortcuts nil
        dashboard-center-content t
        dashboard-set-navigator t
        dashboard-set-init-info t
        dashboard-projects-switch-function 'counsel-projectile-switch-project-by-name

        dashboard-items '((recents        . 5)
                          (projects       . 5)
                          (bookmarks      . 5)
                          (agenda         . 5)
                          (registers      . 5)))
  :custom-face
  (dashboard-heading ((t (:foreground "#f1fa8c" :weight bold))))
  :hook
  (after-init . dashboard-setup-startup-hook))

;; comment ON/OFF to DEBUG init
;; (setq debug-on-error t)

;; Encoding
(prefer-coding-system 'utf-8-unix)
(set-locale-environment "en_US.UTF-8")
(set-default-coding-systems 'utf-8-unix)
(set-selection-coding-system 'utf-8-unix)
(set-buffer-file-coding-system 'utf-8-unix)
(set-clipboard-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
;;________________________________________________________________
;;    ORG MODE
;;________________________________________________________________
(setq org-latex-inputenc-alist '(("utf8" . "utf8x")))
(when window-system (global-prettify-symbols-mode t))

(use-package org
  :config
  (setq org-ellipsis " ⤵") ;; ↴, ▼, ▶, ⤵, ▾
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1))

;; Improve org mode looks
(setq org-roam-v2-ack t                 ; anonying startup message
      org-startup-indented t
      org-pretty-entities t
      org-hide-emphasis-markers t
      org-startup-with-inline-images t
      org-image-actual-width '(300)
      org-log-done 'time                ; I need to know when a task is done
      )

(require 'org-tempo)

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

;; Replace list hyphen with dot
(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(dolist (face '((org-level-1 . 1.2)
                (org-level-2 . 1.1)
                (org-level-3 . 1.05)
                (org-level-4 . 1.0)
                (org-level-5 . 1.1)
                (org-level-6 . 1.1)
                (org-level-7 . 1.1)
                (org-level-8 . 1.1)))
  (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))

;; Make sure org-indent face is available
(require 'org-indent)

;; Ensure that anything that should be fixed-pitch in Org files appears that way
(set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
(set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
(set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

;; (use-package org-appear
;;   :hook (org-mode . org-appear-mode))

;; Beautify Org Checkbox Symbol
(defun teddy-ma/org-buffer-setup ()
  "Something for like document, i guess 😕."
  (push '("[ ]" . "☐" ) prettify-symbols-alist)
  (push '("[X]" . "☑" ) prettify-symbols-alist)
  (push '("[-]" . "❍" ) prettify-symbols-alist)
  )
(add-hook 'org-mode-hook #'teddy-ma/org-buffer-setup)

(defun my/org-mode/load-prettify-symbols ()
  "Looking pretty good, so i adopted it."
  (interactive)
  (setq prettify-symbols-alist
        (mapcan (lambda (x) (list x (cons (upcase (car x)) (cdr x))))
                '(("#+begin_src" . ?)
                  ("#+end_src" . ?)
                  ("#+begin_example" . ?)
                  ("#+end_example" . ?)
                  ("#+header:" . ?)
                  ("#+name:" . ?﮸)
                  ("#+title:" . "")
                  ("#+results:" . ?)
                  ("#+call:" . ?)
                  (":properties:" . ?)
                  (":logbook:" . ?)
                  ("->" . ?➡)))))
(add-hook 'org-mode-hook #'my/org-mode/load-prettify-symbols)

;; (setq-default prettify-symbols-alist '(("#+BEGIN_SRC" . "»") ("#+END_SRC" . "«")("#+begin_src" . "»") ("#+end_src" . "«") ("lambda"  . "λ") ("->" . "→")))
;; Exported html should have no default style. I can style it myself:
(setq org-html-head-include-default-style nil
      org-html-htmlize-output-type 'css)

;; Execute Org src block
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (js .t )
   (shell . t)))

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

(global-unset-key (kbd "<escape>"))
(global-set-key (kbd "<escape>") (kbd "C-g"))

(define-key esc-map "&" 'query-replace-regexp)		; redefined ESC-&
(global-set-key (kbd "M-#") 'query-replace-regexp)
(global-set-key (kbd "M-\"") 'insert-pair)			; Wrap text in quotes
                                        ;(global-set-key (kbd "TAB") 'self-insert-command)	; To make sure that emacs is actually using TABS instead of SPACES

;; I use C-h for backspace in Emacs and move `help-command' elsewhere:
(global-unset-key (kbd "C-h"))
(global-set-key "\C-h" 'backward-delete-char-untabify)
(define-key isearch-mode-map "\C-h" 'isearch-delete-char)
;; (global-set-key (kbd "C-r") 'counsel-minibuffer-history)
(global-set-key (kbd "C-S-H") 'kill-whole-line)
(global-set-key "\C-c\C-c" 'comment-or-uncomment-region)
(global-set-key "\C-t" 'toggle-truncate-lines) ; this lets us have long lines go off the side of the screen instead of hosing up the ascii art
(global-set-key "\C-c\C-d" "\C-a\C- \C-n\M-w\C-y")	; Duplicate a whole line
(global-set-key (kbd "C-S-R") 'rename-file)
(global-set-key "\C-cD" 'Delete-current-file)
(global-set-key "\C-z" 'eshell)
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
  "Go to previous window."
  (interactive)
  (other-window -1))

;; spell check for Bangla text
(global-set-key (kbd "C-c B")
                (lambda()(interactive)
                  (ispell-change-dictionary "bn_BD")
                  (flyspell-buffer)))
;; Toggle show-trailing-whitespace.
(global-set-key (kbd "C-c M-w") (function (lambda () (interactive) (setq show-trailing-whitespace (not show-trailing-whitespace)))))

(global-set-key (kbd "M-J") (lambda () (interactive) (enlarge-window 1)))
(global-set-key (kbd "M-K") (lambda () (interactive) (enlarge-window -1)))
(global-set-key (kbd "M-H") (lambda () (interactive) (enlarge-window -1 t)))
(global-set-key (kbd "M-L") (lambda () (interactive) (enlarge-window 1 t)))
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
(global-set-key (kbd "M-<f1>") 'emojify-insert-emoji)
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
      display-time-default-load-average nil
      mode-line-compact t)
;; Permanent display of line and column numbers is handy.
(size-indication-mode 1)
(display-time-mode)
(display-battery-mode)

;;________________________________________________________________
;;    Balancing Parentheses
;;________________________________________________________________
(electric-pair-mode 1)			    ; auto close bracket insertion
;; make electric-pair-mode work on more brackets
;; (setq electric-pair-pairs
;;       '(
;;         (?\" . ?\")
;;         (?\{ . ?\})))
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
  (define-key dired-mode-map (kbd "C-c x") 'image-dired)
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

(set-frame-parameter nil 'fullscreen 'fullheight)
;;________________________________________________________________
;;    Fonts Setting
;;________________________________________________________________
(global-font-lock-mode 1)               ; Use font-lock everywhere.
(setq font-lock-maximum-decoration t)   ; We have CPU to spare; highlight all syntax categories.

(setq default-input-method "bengali-probhat")
(set-fontset-font "fontset-default" 'bengali (font-spec :family "Kalpurush" :size 16))

(defun remove-quail-show-guidance ()
  "Function for removing guidance."
  nil)
(defun remove-quail-completion ()
  "Function for removing completion."
  (quail-select-current))
(defun bn-company-wordfreq ()
  "Bangla auto-suggestion with company-wordfreq."
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
(set-face-attribute 'font-lock-comment-face nil  :family "Comic Mono" :slant 'italic :height 100)
(set-face-attribute 'font-lock-function-name-face nil :slant 'italic :weight 'bold)
(set-face-attribute 'font-lock-variable-name-face nil :weight 'bold)
(set-face-attribute 'font-lock-keyword-face nil :weight 'bold)

;; (set-face-attribute 'font-lock-comment-face nil :foreground "#5B6268" :slant 'italic)
;; (set-face-attribute 'font-lock-function-name-face nil :foreground "#c678dd" :slant 'italic :weight 'bold)
;; (set-face-attribute 'font-lock-variable-name-face nil :foreground "#dcaeea" :weight 'bold)

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
  "Change cursor color and type according to some minor modes."
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
  "Move cursor back to indentation or to the beginning of the
line if it is already at the indentation.  If it is at the
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
(setq-default backup-by-copying t               ; don't clobber symlinks
              backup-directory-alist `(("."~/.emacs.d/var/backup/per-session))
              default-directory "~/"
              load-prefer-newer t 				; don't use the compiled code if its the older package.
              make-backup-files t               ; backup of a file the first time it is saved.
			  delete-by-moving-to-trash t		; move deleted files to trash
			  delete-old-versions t             ; delete excess backup files silently
			  kept-new-versions 6               ; newest versions to keep when a new numbered backup is made (default: 2)
			  kept-old-versions 2               ; oldest versions to keep when a new numbered backup is made (default: 2)
			  version-control t                 ; version numbers for backup files
			  auto-save-default t               ; auto-save every buffer that visits a file
			  auto-save-timeout 30              ; number of seconds idle time before auto-save (default: 30)
			  auto-save-interval 200            ; number of keystrokes between auto-saves (default: 300)
              show-paren-delay 0           		; how long to wait?
              show-paren-style 'mixed      		; alternatives are 'expression' and 'parenthesis'
              enable-recursive-minibuffers t    ; allow commands to be run on minibuffers.
              tab-width 4
              indent-tabs-mode nil              ; set indentation with spaces instead of tabs with 4 spaces
              indent-line-function 'insert-tab
              require-final-newline t
              save-interprogram-paste-before-kill t
              message-log-max 1000
              fill-column 80
              make-pointer-invisible t          ; hide cursor when writing
              column-number-mode t              ; Show (line,column) in mode-line
              cua-selection-mode t              ; Delete regions
              backward-delete-char-untabify-method 'hungry ; Alternatives is: 'all (remove all consecutive whitespace characters, even newlines)
              )
(save-place-mode t)
(global-auto-revert-mode t)
(setq scroll-preserve-screen-position t
      scroll-margin 3
      scroll-conservatively 101
      inhibit-startup-buffer-menu t
      inhibit-startup-screen t 			; Do not show the startup message.
      visible-bell t					; Flash the screen on error, don't beep.
      view-read-only t					; Toggle ON or OFF with M-x view-mode (or use e to exit view-mode).
      use-dialog-box nil				; Don't pop up UI dialogs when prompting
      delete-auto-save-files t                  ; deletes buffer's auto save file when it is saved or killed with no changes in it.
      save-place-forget-unreadable-files nil
      global-auto-revert-non-file-buffers t		; Revert Dired and other buffers
      frame-title-format '(buffer-file-name "Emacs: %b (%f)" "Emacs: %b")   ; name of the file I am editing as the name of the window
      )

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
  "Comment with '─' (C-x 8 RET BOX DRAWINGS LIGHT HORIZONTAL) on each side."
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
(setq user-full-name       "Likhon Barai"
      user-login-name      "Likhon.Art"
      user-real-login-name "Likhon"
      user-mail-address    "likhonhere007@gmail.com")
;;________________________________________________________________
;;		Highlight Current LINE
;;________________________________________________________________
(when window-system (global-hl-line-mode 1))
;; (set-face-background 'highlight "#3e4446")	; you canalso try: "#3e4446" or "#gray6" etc.
;; (set-face-foreground 'highlight nil)
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
  "Crave for transparency!"
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
(eval-and-compile
  (add-to-list 'use-package-keywords :doc t)
  (defun use-package-handler/:doc (name-symbol _keyword _docstring rest state)
    "An identity handler for :doc.
     Currently, the value for this keyword is being ignored.
     This is done just to pass the compilation when :doc is
     included Argument NAME-SYMBOL is the first argument to
     `use-package' in a declaration.  Argument KEYWORD here is
     simply :doc.  Argument DOCSTRING is the value supplied for
     :doc keyword.  Argument REST is the list of rest of the
     keywords.  Argument STATE is maintained by `use-package' as
     it processes symbols."

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
;; Diminish a feature that removes certain minor-modes from mode-line.
(use-package diminish)

;; Font lock of special Dash variables (it, acc, etc.). Comes default with Emacs.
(global-dash-fontify-mode)

(use-package delight
  :delight)

;; Benchmark startup
;; benchmark-init records startup time by package so we can debug. It only records things after it’s initialised, so put as early in config as possible.
(use-package benchmark-init
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(use-package avy
  :bind(("C-'" . 'avy-goto-char)
        ("C-:" . 'avy-goto-char-2)
        ("M-g g" . 'avy-goto-line)
        ("M-g e" . 'avy-goto-word-0)
        ("M-g w" . 'avy-goto-word-1)
        ("C-c C-j" . 'avy-resume))
  :config
  (setq avy-timeout-seconds 0.5)
  (setq avy-ignored-modes
	    '(image-mode doc-view-mode pdf-view-mode exwm-mode)))

(use-package uniquify-files
  :config
  (setq uniquify-buffer-name-style 'reverse)
  (setq uniquify-separator " • ")
  (setq uniquify-after-kill-buffer-p t)
  (setq uniquify-ignore-buffers-re "^\\*"))

(use-package no-littering
  :doc "It’s good to have centralized working datasets storage, to prevent pollution of Emacs config directory."
  :custom
  (no-littering-var-directory (expand-file-name "data/" user-emacs-directory))
  (no-littering-etc-directory (expand-file-name "config/" user-emacs-directory)))

(setq auto-save-file-name-transforms
	  `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

;;; Show recent files in the File menu.
(use-package recentf
  :doc "Recent buffers in a new Emacs session"
  :config
  (setq recentf-auto-cleanup 'never
        recentf-max-saved-items 500
        recentf-max-menu-items 60)
  (recentf-mode 1)
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory)
  :delight)

(use-package magit
  :doc "Git integration for Emacs"
  :bind ("C-x g" . magit-status)
  :delight)

(use-package git-gutter
  :diminish
  :hook ((prog-mode org-mode) . git-gutter-mode )
  ;;✘
  :config
  (setq git-gutter:modified-sign "†")
  (setq git-gutter:added-sign "†")
  (setq git-gutter:deleted-sign "†")
  (set-face-foreground 'git-gutter:added "Green")
  (set-face-foreground 'git-gutter:modified "Gold")
  (set-face-foreground 'git-gutter:deleted "Red"))

(use-package ibuffer
  :doc "Better buffer management"
  :bind ("C-x C-b" . ibuffer)
  :delight)

(use-package all-the-icons
  :if (display-graphic-p))

;; :config (all-the-icons-install-fonts 'install-without-asking))
;; (cl-defun all-the-icons-faicon (icon &rest _)
;;   #("" 0 1 (rear-nonsticky t display (raise -0.24) font-lock-face (:family "FontAwesome" :height 1.2) face (:family "FontAwesome" :height 1.2)))))

;; Nyan Cat is lovely, it can live on mode line
(use-package nyan-mode
  :init
  (setq nyan-animate-nyancat t)
  (setq nyan-wavy-trail t)
  (setq nyan-minimum-window-width 80)
  (setq nyan-bar-length 75)
  (nyan-mode))

(use-package which-key
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

  ;; Prevent undo tree files from polluting your git repo
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/var/undo-tree-hist")))

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
  :init
  (add-hook 'prog-mode-hook #'aggressive-indent-mode)
  :delight)

;; Opening Files Externally
(use-package openwith
  :config
  (setq openwith-associations
        (list
         (list (openwith-make-extension-regexp
                '("mpg" "mpeg" "mp3" "mp4"
                  "avi" "wmv" "wav" "mov" "flv"
                  "ogm" "ogg" "mkv"))
               "mpv"
               '(file))
         (list (openwith-make-extension-regexp
                '("xbm" "pbm" "pgm" "ppm" "pnm"
                  "png" "gif" "bmp" "tif" "jpeg")) ;; Removed jpg because Telega was
               ;; causing feh to be opened...
               "sxiv"
               '(file))
         (list (openwith-make-extension-regexp
                '("pdf"))
               "zathura"
               '(file)))))

(use-package beacon
  :init
  (beacon-mode t)
  (setq beacon-color "#50D050"))

(use-package emojify
  :hook (erc-mode . emojify-mode)
  :commands emojify-mode)

(use-package alert
  :commands alert
  :config
  (setq alert-default-style 'notifications))

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t 	 ; if nil, bold is universally disabled
        doom-themes-enable-italic t  ; if nil, italics is universally disabled
        doom-themes-treemacs-theme "doom-atom") ; doom-atom or use "doom-colors" for less minimal icon theme
  (load-theme 'doom-gruvbox t)
  ;; (load-theme 'doom-zenburn t)
  (doom-themes-visual-bell-config)	 ; Enable flashing mode-line on errors
  (doom-themes-org-config)		  ; Corrects (and improves) org-mode's native fontification.

  :custom-face
  (company-tooltip ((t (:background "#1d2432"))))
  '(flycheck-warning ((t (:background "#282828" :underline "#fabd2f"))))
  '(org-date ((t (:inherit fixed-pitch))))
  '(ivy-posframe-border ((t (:background "#ffffff")))))

(use-package minions
  :hook (doom-modeline-mode . minions-mode))

(use-package doom-modeline
  :hook
  (after-init . doom-modeline-mode)
  :init
  (setq doom-modeline-env-enable-python nil
        doom-modeline-env-enable-go nil
        doom-modeline-buffer-encoding 'nondefault
        doom-modeline-hud t)
  :config
  (setq doom-modeline-minor-modes t
        find-file-visit-truename t
        doom-modeline-irc nil
        doom-modeline-bar t
        doom-modeline-warning t
        doom-modeline-highlight t
        doom-modeline-persp-icon t
        doom-modeline-persp-name t
        doom-modeline-battery-charging t
        doom-modeline-battery-full t
        doom-modeline-battery-normal t
        doom-modeline-battery-critical t
        doom-modeline-battery-error t
        doom-modeline-buffer-color-icon 1
        doom-modeline-display-misc-in-all-mode-lines t
        doom-modeline-buffer-state-icon t
        doom-modeline-major-mode-color-icon t
        ;; doom-modeline-lsp t
        ;; doom-modeline-height 1
        ;; doom-modeline-bar-width 3
        doom-modeline-buffer-file-name-style 'relative-from-project
        doom-modeline-project-detection 'project))

(use-package ivy
  :doc "A generic completion mechanism"
  :config
  (ivy-mode t)
  (setq ivy-use-virtual-buffers t
        ;; Display index and count both.
        ivy-count-format " (%d/%d) "
        ;; By default, all ivy prompts start with `^'. Disable that.
        ivy-initial-inputs-alist nil)

  :bind (("C-x b" . ivy-switch-buffer)
         ("C-x B" . ivy-switch-buffer-other-window)
         ("<f6>" . ivy-resume)
  		 ("C-c v" . ivy-push-view)
  		 ("C-c V" . ivy-pop-view)
         ("C-c C-r" . ivy-resume))
  :delight)
(use-package ivy-avy)
(use-package ivy-hydra)
(use-package ivy-rich
  :doc "Have additional information in empty space of ivy buffers."
  :custom
  (ivy-rich-path-style 'abbreviate)
  :config
  (setcdr (assq t ivy-format-functions-alist)
          #'ivy-format-function-line)
  (ivy-rich-mode 1)
  :delight)

(use-package ivy-posframe
  :doc "Custom positions for ivy buffers."
  :after ivy
  :custom
  (ivy-posframe-border-width 6)
  :config
  (when (member "Hasklig" (font-family-list))
    (setq ivy-posframe-parameters
          '((font . "Hasklig"))))
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
  :bind (("C-s" . swiper-isearch)) ; ("C-s" . swiper))
  :delight)

;; it looks like counsel is a requirement for swiper
(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("M-y" . counsel-yank-pop)
         ("C-x b" . counsel-switch-buffer)
         ("C-c c" . counsel-compile)
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
		 ("<f2> u" . counsel-unicode-char))
                                        ; ("C-c /" . counsel-ag)
                                        ; ("C-c s" . counsel-rg)
                                        ; ("C-S-o" . counsel-rhythmbox)
  (:map counsel-find-file-map
        ("RET" . ivy-alt-done))
  :delight)

(use-package projectile
  :config
  (projectile-mode +1))
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(use-package expand-region
  :bind (("C-=" . er/expand-region)
         ("C-(" . er/mark-outside-pairs)))

(use-package flycheck
  :diminish
  :hook
  (prog-mode . flycheck-mode)
  :config
  (setq flycheck-indication-mode 'right-fringe)
  ;; Set fringe style
  (when (fboundp 'define-fringe-bitmap)
    (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
      [16 48 112 240 112 48 16] nil nil 'center))

  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (setq flycheck-check-syntax-automatically '(save mode-enabled))

  :bind
  ((:map flycheck-error-list-mode-map
	     ("q" . delete-window)
	     ("j" . flycheck-error-list-next-error)
	     ("k" . flycheck-error-list-previous-error)))
  :custom-face
  (flycheck-warning ((t (:underline (:color "#fabd2f" :style line :position line)))))
  (flycheck-error ((t (:underline (:color "#fb4934" :style line :position line)))))
  (flycheck-info ((t (:underline (:color "#83a598" :style line :position line))))))

(use-package flycheck-clang-tidy
  :after flycheck
  :hook
  (flycheck-mode . flycheck-clang-tidy-setup))

;; Origami Mode (Folding)
;; (use-package origami
;;   :hook (prog-mode . origami-mode))

(add-hook 'prog-mode-hook 'hs-minor-mode)

;; code folding
(global-set-key (kbd "C-<tab>") 'hs-toggle-hiding)  ; fold the current section
(global-set-key (kbd "<backtab>") 'hs-hide-level)  ; fold the sub sections of the current section

;; Center text in the frame, looks nice ;)
(use-package olivetti
  :diminish
  :hook (text-mode . olivetti-mode)
  :hook (prog-mode . olivetti-mode)
  :hook (Info-mode . olivetti-mode)
  :config
  (setq olivetti-body-width 130))

(use-package company-box
  :hook (company-mode . company-box-mode))
;; (set-face-background 'company-box--apply-color "#555555")
(use-package company
  :diminish company-mode
  :hook
  (after-init . global-company-mode)
  :bind
  (:map company-active-map
        ("C-h"        . nil)
        ("C-j"        . nil)
        ("C-k"        . nil)
        ("C-n"        . nil)
        ("C-p"        . nil)
        ("C-w"        . nil)
        ("RET"        . nil)
        ("<return>"   . nil)
        ("SPC"        . nil)    ; Prevent SPC from ever triggering a completion.
        ("M-n"        . nil)
        ("M-p"        . nil)
        ("M-."        . company-show-location)
        ("M-<"        . company-select-first)
        ("M->"        . company-select-last)
        ("C-c C-/"    . company-other-backend)
                                        ;        ("C-<return>"        . company-complete-selection)
        ("C-l"        . company-complete-selection)
        ("<tab>"      . company-indent-or-complete-common)
        ("TAB"        . company-indent-or-complete-common)
        ("C-j"        . company-select-next)
        ("C-k"        . company-select-previous)
        ("C-d"        . company-show-doc-buffer)
        ("C-s"        . company-filter-candidates))
  (:map company-search-map    ; applies to `company-filter-map' too
        ("C-h"        . nil)
        ("C-j"        . nil)
        ("C-k"        . nil)
        ("C-n"        . nil)
        ("C-p"        . nil)
        ("M-/"        . company-complete)
        ("M-TAB"      . company-yasnippet)
        ("C-j"        . company-select-next)
        ("C-k"        . company-select-previous)
        ("C-s"        . company-filter-candidates)
        ([escape]     . company-search-abort))
  :init
  (setq company-idle-delay 0.0
        company-echo-delay 0
        completion-ignore-case t
        company-require-match nil
        company-show-quick-access t
        company-selection-wrap-around t
        company-minimum-prefix-length 1
        company-dabbrev-code-modes t
        company-dabbrev-code-everywhere t
        company-dabbrev-ignore-case nil
        company-debbrev-other-buffers 'all
        company-dabbrev-downcase nil
        company-tooltip-limit 10
        company-tooltip-align-annotations t
        company-tooltip-minimum company-tooltip-limit
        company-begin-commands '(self-insert-command)
        company-require-match #'company-explicit-action-p
        company-frontends '(company-pseudo-tooltip-frontend)
        company-transformers '(company-sort-by-occurrence))
  :config
  (setq company-backends
        '((
           company-files        ; files & directory
           company-keywords     ; keywords
           company-capf         ; what is this?
           company-semantic
           company-gtags
           company-etags
           company-rtags
           company-elisp
           company-ispell
           company-c-headers
           ;; company-clang     ; it's too slow
           ;; company-irony-c-headers
           ;; company-irony
           company-cmake
           company-yasnippet
           company-dabbrev-code)
          (company-abbrev company-dabbrev))))
;; Use tab key to cycle through suggestions. ('tng' means 'tab and go')
                                        ; (company-tng-configure-default)
;; (setq company-backends
;;       '((
;;          company-files        ; files & directory
;;          company-keywords     ; keywords
;;          company-capf         ; what is this?
;;          company-yasnippet
;;          company-dabbrev-code)
;;         (company-abbrev company-dabbrev))))
(use-package company-rtags)
(use-package company-c-headers)

;; Delete duplicates from company popups
(setq-local company-transformers '(delete-dups)
            company-backends '(company-files (:separate company-dabbrev company-ispell)))

;; (use-package irony
;;   :config
;;   (add-hook 'c++-mode-hook 'irony-mode)
;;   (add-hook 'c-mode-hook 'irony-mode)
;;   (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(use-package company-quickhelp
  :after company
  :config
  (setq company-quickhelp-idle-delay 0.0)
  (company-quickhelp-mode 1))

;; This package adds usage-based sorting to Company
;; completions. (Perhaps it too can be replaced by `historian' one day!)
(use-package company-statistics
  :after company
  :config
  (company-statistics-mode))

(use-package yasnippet
  :delight yas-minor-mode " υ"
  :hook (yas-minor-mode . my/disable-yas-if-no-snippets)
  :config (yas-global-mode)
  :preface
  (defun my/disable-yas-if-no-snippets ()
    (when (and yas-minor-mode (null (yas--get-snippet-tables)))
      (yas-minor-mode -1))))

(use-package yasnippet-snippets
  :after yasnippet
  :config (yasnippet-snippets-initialize))

(use-package ivy-yasnippet :after yasnippet)

;; (yas-reload-all)

(add-hook 'prog-mode-hook #'yas-minor-mode)
(setq yas-snippet-dirs '("~/.emacs.d/etc/yasnippet/snippets"))
;; ─────────────────────────────────── C/C++ ─────────────────────────────────── ;;
;; (use-package flycheck-clang-analyzer
;;   :config
;;   (with-eval-after-load 'flycheck
;;     (require 'flycheck-clang-analyzer)
;;      (flycheck-clang-analyzer-setup)))

;; ──────────────────────────────────── SHELL ─────────────────────────────────── ;;

(defun eshell-here ()
  "Opens up a new shell in the directory associated with the
current buffer's file.  The eshell is renamed to match that
directory to make multiple eshell windows easier."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (height (/ (window-total-height) 3))
         (name   (car (last (split-string parent "/" t)))))
    (split-window-vertically (- height))
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))

    (insert (concat "ls"))
    (eshell-send-input)))

(global-set-key (kbd "C-!") 'eshell-here)

(defun eshell/x ()
  "Cmnd `x' exits that shell and closes that window."
  (insert "exit")
  (eshell-send-input)
  (delete-window))

;;; Eshell
;; https://www.masteringemacs.org/article/complete-guide-mastering-eshell Eshell is
;; an elisp shell. It has its own configuration parameters, distinct from those of
;; shell or ansi-terminal.
;;;; Eshell Settings
(setq eshell-directory-name (concat user-emacs-directory "etc/eshell")
      eshell-history-file-name (concat user-emacs-directory "etc/eshell/history")
      eshell-aliases-file (concat user-emacs-directory "etc/eshell/alias")
      eshell-last-dir-ring-file-name (concat user-emacs-directory "etc/eshell/lastdir")
      eshell-prefer-lisp-functions nil
      eshell-highlight-prompt nil
      eshell-buffer-shorthand t
      eshell-cmpl-ignore-case t
      eshell-history-size 10000
      eshell-hist-ignoredups t
      eshell-save-history-on-exit t
      eshell-cmpl-cycle-completions t
      eshell-destroy-buffer-when-process-dies t
      ;; auto truncate after 20k lines
      eshell-buffer-maximum-lines 20000
      eshell-error-if-no-glob t
      eshell-glob-case-insensitive t
      eshell-scroll-to-bottom-on-input 'all
      eshell-destroy-buffer-when-process-dies t
      eshell-list-files-after-cd t
      eshell-banner-message (message "Emacs initialized in %.2fs \n\n" (float-time (time-subtract (current-time) my-start-time))))

;; Visual commands
(setq eshell-visual-commands
      '("ranger" "lfub" "vi" "screen" "top" "less" "more" "lynx"
        "ncftp" "pine" "tin" "trn" "elm" "vim"
        "nmtui" "alsamixer" "htop" "el" "elinks"
        ))
(setq eshell-visual-subcommands '(("git" "log" "diff" "show")))

(add-hook 'shell-mode-hook 'yas-minor-mode)
(add-hook 'shell-mode-hook 'flycheck-mode)
(add-hook 'shell-mode-hook 'company-mode)

(defun shell-mode-company-init ()
  "Company for terminal."
  (setq-local company-backends '((company-shell
                                  company-shell-env
                                  company-etags
                                  company-dabbrev-code))))

(use-package company-shell
  :config
  (require 'company)
  (add-hook 'shell-mode-hook 'shell-mode-company-init))

;; Required for proportional font
(use-package company-posframe
  :config
  (company-posframe-mode t))

(use-package company-wordfreq)
;; ──────────────────────────────── Basic Utils ──────────────────────────────── ;;
;; never want whitespace at the end of lines. Remove it on save.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Browse source tree with Speedbar file browser
(setq speedbar-show-unknown-files t)
;; (setq company-backends (delete 'company-semantic company-backends))

;; ──────────── This snippet loads all *.el files in a directory ─────────── ;;
(defun load-directory (dir)
  "Load all *.el from your .emacs.d directory."
  (let ((load-it (lambda (f)
                   (load-file (concat (file-name-as-directory dir) f)))))
    (mapc load-it (directory-files dir nil "\\.el$"))))
(load-directory "~/.emacs.d/elpa/")

;; ─────────────────────── Open Any File With LineNumber ─────────────────────── ;;
;; i.e. myfile.cpp:12
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


(put 'scroll-left 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;;; Finish up
(provide '.emacs)
;;; .emacs ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
