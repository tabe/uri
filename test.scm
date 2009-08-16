#!r6rs

(import (rnrs) (uri) (xunit))

(define-syntax assert-codec
  (syntax-rules ()
    ((_ decoded encoded)
     (begin
       (assert-string=? decoded (decode-string encoded))
       (assert-string-ci=? encoded (encode-string decoded))))))

(define-syntax assert-string->uri
  (syntax-rules ()
    ((_ (scheme authority path query fragment) str)
     (let ((uri (string->uri str)))
       (assert-equal? scheme (uri-scheme uri))
       (assert-equal? authority (uri-authority uri))
       (assert-equal? path (uri-path uri))
       (assert-equal? query (uri-query uri))
       (assert-equal? fragment (uri-fragment uri))))))

(define-syntax assert-uri->string
  (syntax-rules ()
    ((_ str (scheme authority path query fragment))
     (assert-string=? str (uri->string (make-uri scheme authority path query fragment))))))

(define-syntax assert-string<->uri
  (syntax-rules ()
    ((_ str (scheme authority path query fragment))
     (begin
       (assert-uri->string str (scheme authority path query fragment))
       (assert-string->uri (scheme authority path query fragment) str)))))

(assert-codec " " "%20")
(assert-codec "/path/example" "%2fpath%2fexample")
(assert-codec "省メモリプログラミング" "%e7%9c%81%e3%83%a1%e3%83%a2%e3%83%aa%e3%83%97%e3%83%ad%e3%82%b0%e3%83%a9%e3%83%9f%e3%83%b3%e3%82%b0")
(assert-codec "あいうえお" "%E3%81%82%E3%81%84%E3%81%86%E3%81%88%E3%81%8A")

(assert-string<->uri "http://example.com" ("http" "example.com" "" #f #f))
(assert-string<->uri "http://example.com/" ("http" "example.com" "/" #f #f))
(assert-string<->uri "http://example.com?foo" ("http" "example.com" "" "foo" #f))
(assert-string<->uri "http://example.com?foo#" ("http" "example.com" "" "foo" ""))
(assert-string<->uri "http://example.com?#" ("http" "example.com" "" "" ""))
(assert-string<->uri "http://example.com#bar" ("http" "example.com" "" #f "bar"))
(assert-string<->uri "http://example.com/path/?foo" ("http" "example.com" "/path/" "foo" #f))
(assert-string<->uri "http://user@www.example.com:80/path/" ("http" "user@www.example.com:80" "/path/" #f #f))
(assert-string<->uri "file:///home/foo/bar" ("file" "" "/home/foo/bar" #f #f))
(assert-string<->uri "mailto:foo@example.com" ("mailto" #f "foo@example.com" #f #f))

(report)
