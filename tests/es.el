(require 'ert)
(require 'es)

(ert-deftest es-test-well-documented ()
  (let ((needs-docs nil))
    (mapatoms (lambda (x)
                (when (and (fboundp x)
                           (string-match "^es-" (symbol-name x))
                           (or (not (documentation x t))
                               (string= "" (es-trim (documentation x t)))))
                  (setq needs-docs (cons (symbol-name x) needs-docs)))))
    (should (eql nil (sort needs-docs 'string<)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility function tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ert-deftest es-test-merge-alists ()
  (should (equal '((foo . 5) (baz . 4) (bar . 3))
                 (es-merge-alists '((foo . 1) (bar . 2))
                                  '((bar . 3) (baz . 4))
                                  '((foo . 5))))))

(ert-deftest es-test-to-string ()
  (should (stringp (es-to-string 1)))
  (should (stringp (es-to-string "1")))
  (should (stringp (es-to-string 'foo)))
  (should (stringp (es-to-string '(1 2 3)))))

(ert-deftest es-test-to-list ()
  (should (listp (es-to-list 1)))
  (should (listp (es-to-list "1")))
  (should (listp (es-to-list 'foo)))
  (should (listp (es-to-list '(foo)))))

(ert-deftest es-test-get-in ()
  (let ((l '((a . 1)
             (b . ((c . 2)
                   (d . ((e . 3)
                         (f . 4))))))))
    (should (equal 4 (es-get-in l '(b d f))))
    (should (equal 99 (es-get-in l '(b z d) 99)))
    (should (equal 1 (es-get-in l '(a))))))

(ert-deftest es-test-join ()
  (should (equal "too many secrets" (es-join '("too" "many" "secrets") " ")))
  (should (equal "1, 2, (3 4)" (es-join '(1 2 (3 4)) ", "))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Request function tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ert-deftest es-test-curl-command ()
  (let ((es-curl-program "curl")
        (es-curl-options "--silent -u wagstaff:swordfish"))
    (should (equal "curl --silent -u wagstaff:swordfish -X GET  'endpoint'"
                   (es-curl-command 'get "endpoint")))
    (should (equal "curl --silent -u wagstaff:swordfish -X POST  'endpoint'"
                   (es-curl-command 'post "endpoint")))
    (should (equal (concat "curl --silent -u wagstaff:swordfish "
                           "-X GET -d '{\"size\":1}' 'endpoint'")
                   (es-curl-command 'get "endpoint"
                                    (json-encode '(("size" . 1))))))))

(ert-deftest es-test-query-string ()
  (should (equal "process=true&os=false&tricky=%26%3f%20%2b&number=1"
                 (es-query-string
                  '((process . "true")
                    (os . "false")
                    (tricky . "&? +")
                    (number . 1))))))

(ert-deftest es-test-url ()
  (let ((es-host "127.0.0.1")
        (es-port "9201"))
    (should (equal (concat "127.0.0.1:9201/_health?pretty=true&process=true"
                           "&os=false&tricky=%26%3f%20%2b&number=1")
                   (es-url "_health" '((process . "true") (os . "false")
                                       (tricky . "&? +") (number . 1)))))))

(ert-deftest es-test-make-query ()
  (should (equal '((query (query_string (query . "body:foobar AND count:4"))))
                 (es-make-query "body:foobar AND count:4")))
  (let ((q '((constant_score
              (filter
               (numeric_range
                (age (from . "10") (to . "20")
                     (include_lower . "true") (include_upper . "false"))))))))
    (should (equal q (es-make-query q)))))

(ert-deftest es-test-url-multi-path ()
  (should (equal "index" (es-url-multi-path "index")))
  (should (equal "/a,b,c/" (es-url-multi-path '("a" "b" "c") "/" "/" ",")))
  (should (equal "a,b,c" (es-url-multi-path '("a" "b" "c") nil nil ",")))
  (should (equal "a,b,c/" (es-url-multi-path '("a" "b" "c") nil "/" ",")))
  (should (equal "/a,b,c" (es-url-multi-path '("a" "b" "c") "/" nil ","))))
