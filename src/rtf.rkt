#lang racket
;; ------------------------------------------------------------------
;; Copyright (C) 2013, David Nick Main.
;;
;; See LICENSE file for details of license terms.
;; ------------------------------------------------------------------

(provide rtf->plain)

;; convert rtf to plain text
(define (rtf->plain rtf)
  (if (string? rtf)
      (let* ([filtered (filter (char-filter)
                               (string->list 
                                (regexp-replace* fonttbl-regex rtf "")))]
             [trimmed (trim-space filtered)])
        (list->string trimmed))
      #f))
       
(define fonttbl-regex (regexp "{\\\\fonttbl.*;}"))

(define (char-filter)
  (let ([state 1]) ;; 1=text,2=backslash,3=control-code
    (λ(c)
      (case state
        [(1) (cond ((eq? c #\\) (set! state 2) #f)
                   ((eq? c #\}) #f)
                   ((eq? c #\{) #f)
                   (else #t))]

        [(2) (cond ((eq? c #\\) (set! state 1) #t)
                   ((eq? c #\}) (set! state 1) #t)
                   ((eq? c #\{) (set! state 1) #t)
                   ((eq? c #\newline) (set! state 1) #t)
                   (else (set! state 3) #f))]
        
        [(3) (cond ((eq? c #\space  ) (set! state 1) #f)
                   ((eq? c #\newline) (set! state 1) #f)
                   ((eq? c #\}      ) (set! state 1) #f)
                   ((eq? c #\{      ) (set! state 1) #f)
                   (else #f))]        
        ))))
  
(define (trim-space chars)
  (if (empty? chars) null
      (let ([ch (car chars)]
            [rest (cdr chars)])
        (if (char-whitespace? ch) (trim-space rest)
            (cons ch (trim-space-at-end rest))))))
  
(define (trim-space-at-end chars)
  (foldr (λ(c r) (if (and (null? r) (char-whitespace? c))
                     null
                     (cons c r)))
         null chars))