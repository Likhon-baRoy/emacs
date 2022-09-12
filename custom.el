(custom-set-variables
 '(mode-line-format
   '("%I %e" mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-frame-identification mode-line-buffer-identification "   " mode-line-position
     ((:eval
;;	   (number-to-string (line-number-at-pos (point-max)))
       (number-to-string
        (count-lines
         (point-min)
         (point-max))))
      "L")
     (vc-mode vc-mode)
     "  " mode-line-modes mode-line-misc-info mode-line-end-spaces))
 '(mode-line-position-column-line-format '("(%l,%c)"))
 '(package-selected-packages
   '(irony company-dict auto-complete use-package vline undo-tree bind-key)))
(custom-set-faces
  '(mode-line ((t (:background "black" :foreground "gray70" :box (:line-width 1 :color "black")))))
 )
