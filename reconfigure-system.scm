#!/usr/bin/env guile
!#

(let* ((working-directory (getenv "PWD"))
       (host (string-downcase (gethostname)))
       (path (string-append working-directory
                            "/fuglesteg/"
                            host
                            ".scm")))
  (system* "sudo" "-E" "guix"
           "system" "-L" "." "reconfigure"
           path))
