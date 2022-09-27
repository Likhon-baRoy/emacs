(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mode-line-format
   '("%I %e" mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-frame-identification mode-line-buffer-identification "   " mode-line-position
     ((:eval
       (number-to-string
        (count-lines
         (point-min)
         (point-max))))
      "L")
     (vc-mode vc-mode)
     "  " mode-line-modes mode-line-misc-info mode-line-end-spaces))
 '(mode-line-position-column-line-format '("(%l,%c)"))
 '(package-selected-packages
   '(doom-themes rainbow-delimiters beacon which-key add-hooks windswap irony company-dict auto-complete use-package vline undo-tree bind-key))
 '(tab-width 4))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.

;; '(mode-line ((t (:background "black" :foreground "gray70" :box (:line-width 1 :color "black"))))))

;; rainbow-delimiters-mode setup, with decreasing bracket size
'(rainbow-delimiters-depth-1-face ((t (:foreground "red" :height 1.5))))
'(rainbow-delimiters-depth-2-face ((t (:foreground "orange" :height 1.4))))
'(rainbow-delimiters-depth-3-face ((t (:foreground "yellow" :height 1.3))))
'(rainbow-delimiters-depth-4-face ((t (:foreground "green" :height 1.2))))
'(rainbow-delimiters-depth-5-face ((t (:foreground "blue" :height 1.1))))
'(rainbow-delimiters-depth-6-face ((t (:foreground "violet" :height 1.0))))
'(rainbow-delimiters-depth-7-face ((t (:foreground "chocolate" :height 0.9))))
'(rainbow-delimiters-depth-8-face ((t (:foreground "tomato" :height 0.8))))
'(rainbow-delimiters-unmatched-face ((t (:background "cyan" :height 0.7))))

;; http://xahlee.info/emacs/misc/emacs_rainbow-delimiters-mode.html
 
;; '(rainbow-delimiters-depth-1-face ((t (:foreground "red" :height 2.0))))
;; '(rainbow-delimiters-depth-2-face ((t (:foreground "orange" :height 1.8))))
;; '(rainbow-delimiters-depth-3-face ((t (:foreground "yellow" :height 1.6))))
;; '(rainbow-delimiters-depth-4-face ((t (:foreground "green" :height 1.4))))
;; '(rainbow-delimiters-depth-5-face ((t (:foreground "blue" :height 1.2))))
;; '(rainbow-delimiters-depth-6-face ((t (:foreground "violet" :height 1.1))))
;; '(rainbow-delimiters-depth-7-face ((t (:foreground "purple" :height 1.0))))
;; '(rainbow-delimiters-depth-8-face ((t (:foreground "black" :height 0.9))))
;; '(rainbow-delimiters-unmatched-face ((t (:background "cyan" :height 0.8))))
)
