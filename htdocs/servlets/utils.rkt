#lang racket

;; Gentext
;; ------------------------------------------------------------------------

(define (gentext-println . args)
  (map display args)
  (newline))

(define (gentext-echo . args)
  (map display args))

(define (gentext-key-val-separator)
  (display ":"))

(define (gentext-newline)
  (newline))

(define (gentext-space)
  (gentext-echo " "))

(define (gentext-fancy-line)
  (gentext-println "********************************"))

(define (gentext-format-key-val-pair key val)
  (gentext-echo key)
  (gentext-key-val-separator)
  (gentext-space)
  (gentext-echo val))
  
;; Miscellaneous
;; ------------------------------------------------------------------------

(define (vector-map! v f)
  (define last (- (vector-length v) 1))
  (let loop
    ((i 0))
    (vector-set! v i (f i (vector-ref v i)))
    (unless (>= i last)
      (loop (+ i 1))))
  v)
  
(define (true? pred)
  (eq? pred #t))
(define (false? pred)
  (eq? pred #f))
  
(define (inc n)
  (+ n 1))
(define (dec n)
  (- n 1))

(define (in-range? step start end)
  (cond
    ((and (>= step start)
          (<= step end)) #t)
    (else #f)))


;; Exported Abstractions
;; ------------------------------------------------------------------------
(provide
 vector-map!
 ;make-debugger
 ;dbg-obj
 in-range?
 inc
 dec
 true?
 false?
 gentext-format-key-val-pair
 gentext-newline
 gentext-println
 gentext-fancy-line
 )