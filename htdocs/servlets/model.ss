#lang scheme

(require "utils.rkt")
(require "debugger.rkt")

(define dbg (make-debugger 1))

(define (make-board cols rows)
  (let ((self '())
        (matrix '()))
    
    (define (init)
      (define count -1)
      (set! self dispatch-board)
      (set! matrix (make-vector cols))
      (vector-map!
       matrix
       (lambda (_ __)
         (set! count (+ count 1))
         (make-vector rows count)))
      self)

    (define (position x y)
      (vector-ref (vector-ref matrix x) y))
    
    (define (position! x y val callback)
      (vector-set! (vector-ref matrix x) y val)
      (callback x y val))
    
    (define (map-positions! proc)
      (vector-map!
       matrix
       (lambda (x col)
         (vector-map!
          col
          (lambda (y val)
            (proc x y val)))
         col)))
    
    (define (map-row! y proc)
      (vector-map!
       matrix
       (lambda (x row)
         (proc x y (vector-ref row y))
         row)))
    
    (define (dispatch-board msg)
      (case msg
        ((position) position)
        ((position!) position!)
        ((map-positions!) map-positions!)
        ((map-row!) map-row!)
        ((width) cols)
        ((height) rows)
        (else
         (error "Unknown message:" msg))))
    (init)))

(define (make-space pos)
  (let ((self '())
        (name '())
        (tag '())
        (object '()))
    (define (init)
      (set! self dispatch-space)
      (set! name 'default)
      self)
    (define (attach-object! new-object)
      (set! object new-object))
    (define (occupied?)
      (not (null? object)))
    (define (name! new-name)
      (set! name new-name))
    (define (tag! new-tag)
      (set! tag new-tag))
    (define (dispatch-space msg)
      (case msg
        ((occupied?) (occupied?))
        ((name) name)
        ((name!) name!)
        ((first-object) object)
        ((attach-object!) attach-object!)
        ((position) pos)
        ((tag) tag)
        ((tag!) tag!)
        (else
         (error "Unknown message" msg))))
    (init)))

(define (make-pawn color name rank position rule . args)
  (define self '())
  (define meta '())
  (define (init)
    (set! self dispatch-pawn)
    (when (not (null? args))
      (set! meta (car args)))
    self)
  (define (position! new-pos callback)
    (set! position new-pos)
    (callback new-pos)
    self)
  (define (dispatch-pawn msg)
    (case msg
      ((color) color)
      ((position) position)
      ((position!) position!)
      ((name) name)
      ((rank) rank)
      ((meta) meta)
      (else
       (error "Unknown message to dispatch-pawn" msg))))
  (init))

(define (make-point x y)
  (define self '())
  (define (init)
    (set! self dispatch-point)
    self)
  (define (x! new-x)
    (set! x new-x))
  (define (y! new-y)
    (set! y new-y))
  (define (dispatch-point msg)
    (case msg
      ((x) x)
      ((x!) x!)
      ((y) y)
      ((y!) y!)
      (else
       (error "Unknown message:" msg))))
  (init))

(define (make-meta-pawn)
  (define self '())
  (define pwns '())
  (define (init)
    (set! self dispatch-meta-pawn)
    self)
  (define (new color name rank position rule)
    (define new-pwn (make-pawn color name rank position rule self))
    (dbg 
     'new-meta-pawn 
     (dbg-obj 
      pwns 
      (lambda (pwn)
        (pwn 'name))))
    (set! pwns (cons new-pwn pwns))
    new-pwn)
  (define (dispatch-meta-pawn msg)
    (case msg
      ((new) new)
      (else
       (error "Unknown message: " msg))))
  (init))

(provide
 make-board
 make-space
 make-pawn
 make-point
 make-meta-pawn
 )