
;;; Set search engines
(defvar *my-search-engines*
  (list
    '("gh" "https://github.com/search?q=~a" "https://github.com")
    '("g" "https://google.com/search?q=~a" "https://google.com"))) ; Last engine is default

(define-configuration context-buffer
  ((search-engines
     (append %slot-default%
        (mapcar (lambda (engine) (apply 'make-search-engine engine))
                *my-search-engines*)))))
