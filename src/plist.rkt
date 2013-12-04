#lang racket
;; ------------------------------------------------------------------
;; Copyright (C) 2013, David Nick Main.
;;
;; See LICENSE file for details of license terms.
;; ------------------------------------------------------------------

(require xml/plist)
(require file/gunzip)

(provide load-plist)

;; Load a PLIST file and return a hash of its contents
(define (load-plist path)
  (let* ([file-path (if (directory-exists? path)
                        (build-path path "data.plist")
                        path)]
         [file-port #f]
         [plist #f])
    (set! file-port (open-input-file file-path))
    (with-handlers
        ([exn:fail? (λ(e) 
                      ;; reopen the file and decompress it before re-attempting
                      (close-input-port file-port)
                      (set! file-port (open-input-file file-path))
                      (let-values ([(in out) (make-pipe #f)])
                        (gunzip-through-ports file-port out)
                        (close-output-port out)
                        (set! plist (read-plist in))))])
      (set! plist (read-plist file-port))
      (close-input-port file-port))
    (plist->hash plist)))

(define (port->plist in)
  (plist->hash (read-plist in)))

;; convert plist structure to hashes
(define (plist->hash plist-dict)
  (make-immutable-hash (map (λ(assoc-pair)
                              (cons (cadr assoc-pair)
                                    (plist->val (caddr assoc-pair))))
                            (cdr plist-dict))))

;; normalize a plist value
(define (plist->val val)  
  (match val 
    ['(true)  #t]
    ['(false) #f]
    [`(integer ,i) i]
    [`(real    ,r) r]
    [(cons 'dict    _) (plist->hash val)]
    [(cons 'array els) (map plist->val els)]
    [v v]))
