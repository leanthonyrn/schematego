#lang racket

;; Debugger
;; ------------------------------------------------------------------------

(define *dbg-obj-handlers* '())

(define (dbg-obj-payload payload)
  (define self '())
  (define (init)
    (set! self dispatch-dbg-obj-payload)
    self)
  (define (dispatch-dbg-obj-payload msg)
    (case msg
      ((dbg-obj-payload?) #t)
      ((as-list) payload)
      (else
       (error "Unknown message" msg))))
  (init))

(define (dbg-key-val-obj . args)
  (define self '())
  (define payload '())
  (define usual '())
  (define key '())
  (define val '())
  
  (define (init)
    (when (and (not (null? args))
               (procedure? (car args)))
      (when ((car args) 'dbg-obj-payload?)
        (set! payload ((car args) 'as-list))
        (when (not (null? (cdr args)))
          (set! usual (cadr args)))
        (set! key (car payload))
        (set! val (cadr payload))))
    (set! self dispatch-dbg-key-val-obj)
    self)
  
  (define (qualifier)
    (symbol? key))
  
  (define (formatter)
    (gentext-format-key-val-pair key val)
    (gentext-newline))
  
  (define (dbg-key-val-obj? msg)
    (or 
     (eq? msg (object-name usual))
     (eq? (object-name self)
          'dispatch-dbg-key-val-obj)))
  
  (define (dispatch-dbg-key-val-obj msg)
    (case msg
      ((dbg-key-val-obj?) (dbg-key-val-obj? msg))
      ((key) key)
      ((value) val)
      ((qualifier) qualifier)
      ((formatter) formatter)
      (else
       (usual msg))))
  (init))

(define (dbg-obj . args)
  (define self '())
  (define handlers '())
  (define selector '())
  (define payload '())

  (define (init)
    (set! self dispatch-dbg-obj)
    (set! handlers *dbg-obj-handlers*)
    (set! selector default-selector)
    (set! payload (dbg-obj-payload args))
    (cond 
      ((null? args)
       self)
      (else
       (let* 
           ((handler-instances '())
            (handler-qualification-mask
             (map
              (lambda (handler)
                (let ((handler-instance (handler payload self)))
                  (set! handler-instances 
                        (cons handler-instance handler-instances))
                  ((handler-instance 'qualifier))))
              handlers)))
         (selector handler-instances handler-qualification-mask)))))
    
  (define (default-selector handlers result-of-evaluating-handler-qualifiers)
    ;; Choose the first qualifying handler
    (define selected-handler '())
    (map 
     (lambda (handler qualified?)
       (when qualified?
         (set! selected-handler handler)))
     handlers 
     result-of-evaluating-handler-qualifiers)
    selected-handler)
  
  (define (selector! new-selector)
    (set! selector new-selector))
  
  (define (add-handler! handler . for-extensibility)
    (when (not (procedure? handler))
      (error "A debug object handler must be a procedure"))
    (set! handlers (cons handler handlers)))
  
  (define (dbg-obj?)
    (eq? (object-name self) 'dispatch-dbg-obj))
  
  (define (dispatch-dbg-obj msg)
    (case msg
      ((dbg-obj?) (dbg-obj?))
      ((payload) payload)
      ((add-handler!) add-handler!)
      ((handler-selector!) selector!)
      (else
       (error "Unknow message" msg))))
  (init))

(define (make-debugger level)
  (define range '(0 1))
  (define self '())
  
  (define (init)
    (set! self dispatch-debug)
    self)
  
  (define (level! new-val)
    (when (not (in-range new-val (car range) (last range)))
      (error "Invalid debug level" new-val))
    (set! level new-val))
  
  (define (report context args)
    (gentext-start-of-debug-clause context)
    (map
     (lambda (arg)
       (gentext-println "in cond" args)
       (cond
         ((and (procedure? arg)
               (arg 'dbg-obj?))
          (gentext-println arg " " (arg 'formatter))
          ((arg 'formatter)))
         (else
          (gentext-println arg))))
     args)
    (gentext-end-of-debug-clause))
  
  (define (dispatch-debug msg . args)
    (case msg
      ((level) level)
      ((level!) level!)
      (else
       (report msg args))))
  (init))

(define (gentext-start-of-debug-clause context)
  (gentext-fancy-line)
  (gentext-println  "Start of debug clause")
  (gentext-println  "Context: " context)
  (gentext-fancy-line))

(define (gentext-end-of-debug-clause)
  (gentext-fancy-line)
  (gentext-println  "End of debug clause"))

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

;; Initializing global variables
;; ------------------------------------------------------------------------

(set! 
 *dbg-obj-handlers*
 (list 
  dbg-key-val-obj))


;; Exported Abstractions
;; ------------------------------------------------------------------------
(provide
 vector-map!
 make-debugger
 dbg-obj
 in-range?
 inc
 dec
 )