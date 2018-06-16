;; (let ((files (directory-files-and-attributes "~/.emacs.d/site-lisp" t)))
;;   (dolist (file files)
;;     (let ((filename (car file))
;;           (dir (nth 1 file)))
;;       (when (and dir
;;                  (not (string-suffix-p "." filename)))
;;         (add-to-list 'load-path (car file))))))
;; (require 'show-project-euler)

(add-to-list 'load-path "~/src/github.com/kazu-yamamoto/Goby")
(autoload 'goby "goby" nil t)
