;; packages.lisp

(defpackage #:restman.utility
  (:use #:cl #:alexandria #:iter #:split-sequence #:blackbird)
  (:shadowing-import-from #:iter #:finally)
  (:export #:try-unpromisify
           #:request-hash-table
           #:reply-hash-table
           #:encode-to-file
           #:string-to-symbol
           #:native-namestring
           #:parse-native-namestring))

(defpackage #:restman.spy
  (:use #:cl #:alexandria #:iter #:blackbird #:restman.utility)
  (:shadowing-import-from #:iter #:finally)
  (:export #:spy
           #|-----------------------------------------------------------------|#
           #:spy-genid-style
           #:spy-requests-list
           #:spy-active-p
           #:post-file-directory
           #|-----------------------------------------------------------------|#
           #:start-spy
           #:finish-spy
           #:generate-request-id
           #:make-post-file-name
           #:register-record
           #:export-spy-journal
           #|-----------------------------------------------------------------|#
           #:@spy))

(defpackage #:restman.simulator
  (:use #:cl #:alexandria #:iter #:blackbird #:split-sequence #:restman.utility)
  (:shadowing-import-from #:iter #:finally)
  (:export #:read-environment
           #|-----------------------------------------------------------------|#
           #:simulator
           #|-----------------------------------------------------------------|#
           #:simulator-config
           #:simulator-requests
           #:simulator-children
           #:simulator-last-replies
           #:simulator-last-results
           #|-----------------------------------------------------------------|#
           #:make-child-simulator
           #:load-config
           #:simulator-name
           #:reformat-json-string
           #:reformat-html-string
           #:format-string-diff
           #:format-content-diff
           #:compare-failed
           #:compare-success
           #:compare-http-code
           #:compare-content-type
           #:compare-content-mode
           #:compare-text-content
           #:compare-binary-content
           #:compare-replies
           #:find-correct-reply
           #:format-last-results
           #:run
           #:fixate))
