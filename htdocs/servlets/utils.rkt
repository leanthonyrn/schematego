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

(define (process-dbg-obj-args args)
  (define common-error 
    (string-append 
     "Either 0 or 3 arguments must be provided.\n"
     "If any argument is passed to a debug object, "
     "the argument list MUST have the following structure:\n"
     "an instance of dbg-obj-payload as payload, followed by:\n"
     "an instance of dbg-obj as parent (or usual), followed by:\n"
     "an instance of continuation? as abort.\n\n"))
  (cond 
    ((and (not (null? args))
          (procedure? (car args))
          ((car args) 'dbg-obj-payload?))
     (let ((payload ((car args) 'as-list)))
       (when (null? (cdr args))
         (error common-error "The second argument is missing.\n"))
       (let ((parent (cadr args)))
         (list payload parent))))
    (else
     (list null null null))))

(define (default-dbg-obj . args)
  (define self '())
  (define payload '())
  (define usual '())
  
  (define (init)
    (let ((structured-args (process-dbg-obj-args args)))
      (set! payload (car structured-args))
      (set! usual (cadr structured-args)))
    (set! self dispatch-default-dbg-obj)
    self)
  
  (define (qualifier) #t)
  (define (formatter)
    (gentext-println payload))
  
  (define (dispatch-default-dbg-obj msg)
    (case msg
      ((default-dbg-obj?) #t)
      ((qualifier) qualifier)
      ((formatter) formatter)
      (else
       (usual msg))))
  (init))

(define (key-val-dbg-obj . args)
  (define self '())
  (define payload '())
  (define usual '())
  (define key '())
  (define val '())
  
  (define (init)
    (let ((structured-args (process-dbg-obj-args args)))
      (set! payload (car structured-args))
      (set! usual (cadr structured-args)))
    (set! self dispatch-key-val-dbg-obj)
    self)
  
  (define (qualifier)
    (define result
      (and (not (null? payload))
           (not (null? (cdr payload)))
           (symbol? (car payload))))
    (when result
      (set! key (car payload))
      (set! val (cadr payload)))
    result)
    
  (define (formatter)
    (gentext-format-key-val-pair key val)
    (gentext-newline))
  
  (define (dispatch-key-val-dbg-obj msg)
    (case msg
      ((key-val-dbg-obj?) #t)
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
         (selector 
          (reverse handler-instances) 
          handler-qualification-mask)))))

  (define (default-selector handlers handler-qualification-mask)
    ;; Choose the first qualifying handler
    (define selected-handler '())
    (for-each 
     (lambda (handler qualified?)
       (when (true? qualified?)
         (set! selected-handler handler)))
     handlers 
     handler-qualification-mask)
    selected-handler)
  
  (define (selector! new-selector)
    (set! selector new-selector))
  
  (define (add-handler! handler . for-extensibility)
    (when (not (procedure? handler))
      (error "A debug object handler must be a procedure"))
    (set! handlers (cons handler handlers)))
  
  (define (dispatch-dbg-obj msg)
    (case msg
      ((dbg-obj?) #t)
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
       (cond
         ((and (procedure? arg)
               (arg 'dbg-obj?))
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
  key-val-dbg-obj
  default-dbg-obj))


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