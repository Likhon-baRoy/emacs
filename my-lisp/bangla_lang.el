;;; bangla_lang.el --- Bangla language configurations -*- lexical-binding: t; -*-
;;; Created on: 2022 Dec 18

;; Copyright (C) 2021-2022 Likhon Sapiens <likhonhere007@gmail.com>

;; Author: Likhon Sapiens <likhonhere007@gmail.com>
;; URL: https://github.com/Likhon-baRoy/.emacs.d
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This covers my tweaks for Bangla language that are meant for use in my
;; Emacs setup: https://github.com/Likhon-baRoy/.emacs.d.
;;
;; Remember that every piece of Elisp that I write is for my own
;; educational and recreational purposes.  I am not a programmer and I
;; do not recommend that you copy any of this if you are not certain of
;; what it does.

;;; Code:



(setq default-input-method "bengali-probhat")
(set-fontset-font "fontset-default" 'bengali (font-spec :family "Kalpurush" :size 16))

(use-package company-wordfreq
  :defer t
  :after company
  :delight " 𝛄")

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

(add-hook 'input-method-activate-hook #'bn-company-wordfreq)

;; spell check for Bangla text
(bind-key "C-c B"
          (lambda () (interactive)
            (ispell-change-dictionary "bn_BD")
            (flyspell-region)))

;;; Finish up
(provide 'bangla_lang)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; bangla_lang.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
