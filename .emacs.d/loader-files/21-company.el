(use-package company
  :ensure
  :diminish ""
  :config
  (global-company-mode)
  (setq company-idle-delay 0.3)
  (setq company-minimum-prefix-length 1)
  (setq company-begin-commands '(self-insert-command))
  (setq company-selection-wrap-around t)
  (setq company-show-numbers t)
  (let ((map company-active-map))
    (mapc
     (lambda (x)
       (define-key map (format "%d" x) 'ora/company-number))
     (number-sequence 0 9))
    (define-key map " " (lambda ()
                          (interactive)
                          (company-abort)
                          (self-insert-command 1)))
    (define-key map (kbd "<return>") nil)))

(defun ora/company-number ()
  "Forward to `company-complete-number'."
  (interactive)
  (let* ((k (this-command-keys))
         (re (concat "^" company-prefix k)))
    (if (cl-find-if (lambda (s) (string-match re s))
                    company-candidates)
        (self-insert-command 1)
      (company-complete-number (string-to-number k)))))

