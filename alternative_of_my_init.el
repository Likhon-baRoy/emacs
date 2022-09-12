(defun my-c-mode-common-hook ()
  "my customizations for all of c-mode and related modes"
  (setq c-basic-offset 4)
  (setq c-basic-indent 4))
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;; (defun my-custom-settings-fn ()
;;   (setq tab-width 4)
;;   (setq indent-tabs-mode t)
;;   (setq tab-stop-list (number-sequence 4 200 4)))
;; (add-hook 'text-mode-hook 'my-custom-settings-fn)
