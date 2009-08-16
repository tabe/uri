;;
;;   Copyright (c) 2009 Takeshi Abe. All rights reserved.
;;
;;   Redistribution and use in source and binary forms, with or without
;;   modification, are permitted provided that the following conditions
;;   are met:
;;
;;    1. Redistributions of source code must retain the above copyright
;;       notice, this list of conditions and the following disclaimer.
;;
;;    2. Redistributions in binary form must reproduce the above copyright
;;       notice, this list of conditions and the following disclaimer in the
;;       documentation and/or other materials provided with the distribution.
;;
;;    3. Neither the name of the authors nor the names of its contributors
;;       may be used to endorse or promote products derived from this
;;       software without specific prior written permission.
;;
;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(library (uri)
  (export decode
          decode-string
          encode
          encode-string
          &invalid-percent-encoding
          invalid-percent-encoding?
          invalid-percent-encoding-position
          invalid-percent-encoding-sequence
          make-uri
          uri?
          uri-scheme
          uri-authority
          uri-path
          uri-query
          uri-fragment
          string->uri
          uri->string
          truncated-uri?
          )
  (import (rnrs))

  (define *reserved*
    '#(#\: #\/ #\? #\# #\[ #\] #\@ ; gen-delims
       #\! #\$ #\& #\' #\( #\)     ; sub-delims
       #\* #\+ #\, #\; #\=))

  (define *unreserved*
    '#(#\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M
       #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z
       #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m
       #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z
       #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
       #\- #\. #\_ #\~))

  (define (unreserved? c)
    (call/cc
     (lambda (found)
       (vector-for-each
        (lambda (r)
          (if (char=? c r)
              (found #t)))
        *unreserved*)
       #f)))

  (define (hex-digit? c)
    (or (char<=? #\0 c #\9)
        (char<=? #\a c #\f)
        (char<=? #\A c #\F)))

  (define-condition-type &invalid-percent-encoding &condition
    make-invalid-percent-encoding invalid-percent-encoding?
    (position invalid-percent-encoding-position)
    (sequence invalid-percent-encoding-sequence))

  (define (decode iport oport)
    (assert (textual-port? iport))
    (assert (binary-port? oport))
    (let lp ((n 0)
             (c (get-char iport)))
      (cond ((eof-object? c)
             n)
            ((char=? c #\%)
             (let ((x (get-string-n iport 2)))
               (cond ((eof-object? x)
                      (raise (make-invalid-percent-encoding n c)))
                     ((and (= 2 (string-length x))
                           (hex-digit? (string-ref x 0))
                           (hex-digit? (string-ref x 1)))
                      (let ((i (string->number x 16)))
                        (put-u8 oport i)
                        (lp (+ n 1) (get-char iport))))
                     (else
                      (raise (make-invalid-percent-encoding n (cons c (string->list x))))))))
            (else
             (put-u8 oport (char->integer c))
             (lp (+ n 1) (get-char iport))))))

  (define (decode-string str)
    (assert (string? str))
    (call-with-port (open-string-input-port str)
      (lambda (iport)
        (utf8->string
         (call-with-bytevector-output-port
          (lambda (oport)
            (decode iport oport)))))))

  (define (encode iport oport)
    (assert (binary-port? iport))
    (assert (textual-port? oport))
    (let lp ((n 0)
             (b (get-u8 iport)))
      (if (eof-object? b)
          n
          (cond ((and (< b #x80)
                      (let ((c (integer->char b)))
                        (and (unreserved? c) c)))
                 => (lambda (c)
                      (put-char oport c)
                      (lp (+ n 1) (get-u8 iport))))
                (else
                 (put-char oport #\%)
                 (if (< b 16) (put-char oport #\0))
                 (put-string oport (number->string b 16))
                 (lp (+ n 3) (get-u8 iport)))))))

  (define (encode-string str)
    (assert (string? str))
    (call-with-port (open-bytevector-input-port (string->utf8 str))
      (lambda (iport)
        (call-with-string-output-port
         (lambda (oport)
           (encode iport oport))))))

  (define-record-type uri
    (fields scheme
            authority
            path
            query
            fragment))

  (define-condition-type &truncated-uri &condition
    make-truncated-uri truncated-uri?)

  (define (string->uri str)
    (assert (string? str))
    (call-with-port (open-string-input-port str)
      (lambda (port)

        (define-syntax until-next
          (syntax-rules ()
            ((_ proc (c0 proc0) ...)
             (let loop ((c (get-char port))
                        (r '()))
               (cond ((eof-object? c)
                      (proc (list->string (reverse r))))
                     ((char=? c0 c)
                      (proc0 (list->string (reverse r))))
                     ...
                     (else
                      (loop (get-char port) (cons c r))))))))

        (define (get-fragment scheme authority path query)
          (until-next
           (lambda (fragment) (make-uri scheme authority path query fragment))))

        (define (get-query scheme authority path)
          (until-next
           (lambda (query) (make-uri scheme authority path query #f))
           (#\#
            (lambda (query) (get-fragment scheme authority path query)))))

        (define (get-path scheme authority head)
          (until-next
           (lambda (tail)
             (make-uri scheme authority (string-append head tail) #f #f))
           (#\?
            (lambda (tail)
              (get-query scheme authority (string-append head tail))))
           (#\#
            (lambda (tail)
              (get-fragment scheme authority (string-append head tail) #f)))))

        (define (get-authority scheme)
          (until-next
           (lambda (authority) (make-uri scheme authority "" #f #f))
           (#\/
            (lambda (authority) (get-path scheme authority "/")))
           (#\?
            (lambda (authority) (get-query scheme authority "")))
           (#\#
            (lambda (authority) (get-fragment scheme authority "" #f)))))

        (until-next
         (lambda _ (raise (make-truncated-uri)))
         (#\:
          (lambda (scheme)
            (let ((str (get-string-n port 2)))
              (cond ((eof-object? str)
                     (make-uri scheme #f "" #f #f))
                    ((string=? "//" str)
                     (get-authority scheme))
                    (else
                     (get-path scheme #f str))))))))))

  (define (uri->string uri)

    (define (prefixed-if-any x prefix)
      (if (string? x)
          (string-append prefix x)
          ""))

    (assert (uri? uri))
    (string-append (uri-scheme uri)
                   ":"
                   (prefixed-if-any (uri-authority uri) "//")
                   (uri-path uri)
                   (prefixed-if-any (uri-query uri) "?")
                   (prefixed-if-any (uri-fragment uri) "#")))

)
