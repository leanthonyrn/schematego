#lang racket

(require web-server/servlet)
(provide/contract (start (request? . -> . response/c)))

(require web-server/private/url-param)
(require "utils.rkt")
(require "model.ss")

(define board (make-board 10 10))

(define (make-pawns)
  (define meta-pwn (make-meta-pawn))
  (define names 
    '(marshal general colonel major captain lieutenant sergeant miner scout 
              spy bomb flag))
  (define ranks '(10 9 8 7 6 5 4 3 2 1 0 0))
  (define colors (map (lambda (_) 'red) names))
  (define positions (map (lambda (_) (make-point 0 0)) names))
  (define rules (map (lambda (_) (lambda () #t)) names))
  (map
   (lambda (color name rank pos rule)
     ((meta-pwn 'new) color name rank pos rule))
   colors names ranks positions rules))

(define pawns (make-pawns))
(define selected-pawn '())

(define (to-string any)
  (cond
    ((symbol? any)
     (symbol->string any))
    ((number? any)
     (number->string any))
    (else
     any)))

(define (to-bytes any)
  (string->bytes/utf-8 (to-string any)))
  
(define (board-position-clicked-handler request)
  (define binds (request-bindings request))
  (define (get-binding sym)
    (string->number (extract-binding/single sym binds)))
  (define x (get-binding 'x))
  (define y (get-binding 'y))
  ((((board 'position) x y) 'attach-object!) selected-pawn)
  (render-main-page 
   request 
   " The pawn is added to the board position you chose"))

(define (render-board-position space trap)
  (define name (to-string (space 'name)))
  (define pos (space 'position))
  (define clsval
    (let ((tag (space 'tag)))
      (if (null? tag)
          "space"
          (string-append "space " (to-string tag)))))
  `(td 
    ((class ,clsval))     
    (a 
     ((href 
       ,(format 
         "~a?x=~a&y=~a"
         (trap board-position-clicked-handler)
         (pos 'x)
         (pos 'y))))
     (span 
      ((class "pawn"))
      ,(if (space 'occupied?)
           (symbol->string ((space 'first-object) 'name))
           " ")))))

(define (render-board-row row trap)
  (define positions '())
  ((board 'map-row!)
   row
   (lambda (x y space)
     (set! 
      positions 
      (cons space positions))
     space))
  (set! positions (reverse positions))
  `(tr ,@(map render-board-position positions
              (map (lambda (_) trap) positions))))

(define (render-board trap)
  (define rows '())
  (let loop
    ((i 0))
    (unless (>= i (board 'height))
      (set! 
       rows 
       (cons (render-board-row i trap) rows))
      (loop (+ i 1))))
  `(table
    ((id "board-table"))
    ,@rows))

(define (init-board)
  (define (lake? x y)
    (define x-lakes (list 2 2 3 3 6 6 7 7))
    (define y-lakes (list 4 5 4 5 4 5 4 5))
    (define (accum val lst)
      (if (null? lst)
          val
          (accum (or val (car lst)) (cdr lst))))
    (accum 
     #f
     (map
      (lambda (x-lake y-lake)
        (and (eq? x x-lake)
             (eq? y y-lake)))
      x-lakes y-lakes)))
  
  (define (red? x y)
    (and (in-range? x 0 9)
         (in-range? y 0 3)))
  (define (blue? x y)
    (and (in-range? x 0 9)
         (in-range? y 6 9)))
  ((board 'map-positions!)
   (lambda (x y val)
     (let ((space (make-space (make-point x y)))
           (name 
            (string->symbol
             (apply 
              string-append 
              (map to-string `(,x - ,y))))))
       ((space 'name!) name)
       (cond 
         ((lake? x y)
          ((space 'tag!) 'lake))
         ((red? x y)
          ((space 'tag!) 'red))
         ((blue? x y)
          ((space 'tag!) 'blue))
         (else
          ((space 'tag!) '())))
       space))))

(define (pawn-clicked-handler request)
  (define pwn-name (extract-binding/single 'pwn (request-bindings request)))
  (set! selected-pawn (make-pawn 'red (string->symbol pwn-name) 10 (make-point 0 0) (lambda () #t)))
  (render-main-page request (string-append pwn-name " selected")))

(define (render-pawn pwn trap)
  (define pwn-name (to-string (pwn 'name)))
  `(li
    (form
     ((action ,(trap pawn-clicked-handler)))
     (input
      ((name "pwn")
       (value ,pwn-name)))
     (input
      ((type "submit")
       (value "select")
       (class "confirm"))))))

(define (render-pawns trap)
  `(div 
    ((id "pawns"))
    (h2 "Pawns")
    (ul 
     ,@(map render-pawn pawns (map (lambda (_) trap) pawns)))))
  
(define (render-header)
  '(head
    (meta
     ((http-equiv "content-type")
      (content "text/html; charset=utf-8")))
    (link
     ((rel "stylesheet")
      (href "/css/base.css")
      (type "text/css")
      (media "screen")))
    (title "Schematego")))

(define (render-user-message-box umsg)
  `(div
    ((id "messages")) (h2 "Messages")
    ,umsg))

(define (render-main-page request umsg)
  (send/suspend/dispatch
   (lambda (trap)
     `(html
       ,(render-header)
       (body
        (h1 "Schematego")
        (div 
         ((id "sidebar"))
         ,(render-user-message-box umsg)
         ,(render-pawns trap))
        (div
         ((id "board"))
         ,(render-board trap)))))))

(define (start request)
  (init-board)
  (render-main-page request "Welcome!"))

(require web-server/servlet-env)
(serve/servlet 
 start
 #:launch-browser? #f
 #:quit? #f
 #:listen-ip #f
 #:port 8000
 #:extra-files-paths
 (list 
  (build-path 
   (find-system-path 'home-dir)
   "ws/schematego/htdocs"))
 #:servlet-path
 "/servlets/schematego.rkt")