(require 'flycheck)

(defconst squee--keywords
  '("def" "export"))

(defconst squee--font-lock
  `((,(concat "\\(def\\|export\\)\\s-+\\(" lisp-mode-symbol-regexp "\\)") . ((2 font-lock-function-name-face)))
    (,(concat "\\_<" (regexp-opt squee--keywords) "\\_>") . font-lock-keyword-face)))

;;;###autoload
(define-derived-mode squee-mode prog-mode "Squee"
  "Major mode for editing Squee files"
  (setq-local compile-command "squee check")
  (setq font-lock-defaults '(squee--font-lock)))

;;;###autoload
(add-to-list 'auto-mode-alist (cons (purecopy "\\.squee\\'") 'squee-mode))

(define-key squee-mode-map (kbd "C-c C-c") #'compile)


;;; Checker

(flycheck-define-checker squee
  "A checker for Squee source files"
  :command ("squee" "check" source)
  :modes (squee-mode)
  :error-patterns
    ((error line-start (file-name) ":" line ":" column ": " (message))
     (error line-start (file-name) ":" (message))))

(add-to-list 'flycheck-checkers 'squee)
(add-hook 'squee-mode-hook 'flycheck-mode)

(provide 'squee)
