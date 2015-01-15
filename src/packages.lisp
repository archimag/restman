;; packages.lisp

(defpackage #:restman.utility
  (:use #:cl #:alexandria #:iter #:split-sequence #:blackbird)
  (:shadowing-import-from #:iter #:finally)
  (:export #:try-unpromisify
           #:request-hash-table
           #:reply-hash-table
           #:encode-to-file))

(defpackage #:restman.spy
  (:use #:cl #:alexandria #:iter #:blackbird #:restman.utility)
  (:shadowing-import-from #:iter #:finally)
  (:export #:spy
           #:spy-requests-list
           #:spy-active-p
           #:start-spy
           #:finish-spy
           #:generate-request-id
           #:export-spy-journal
           #:@spy))

(defpackage #:restman.simulator
  (:use #:cl #:alexandria #:iter #:blackbird #:split-sequence #:restman.utility)
  (:shadowing-import-from #:iter #:finally)
  (:export #:read-environment
           #:simulator
           
           #:simulator-requests
           #:find-correct-reply
           #:simulator-last-replies
           
           #:print-run-results
           #:run #:fixate))

