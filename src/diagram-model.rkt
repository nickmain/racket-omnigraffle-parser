#lang racket
;; ------------------------------------------------------------------
;; Copyright (C) 2013, David Nick Main.
;;
;; See LICENSE file for details of license terms.
;; ------------------------------------------------------------------

;; Model for Omnigraffle diagrams
(provide (all-defined-out))

(struct Document (filename canvas-hash canvases) 
  #:transparent)

(struct Canvas 
  (id 
   title 
   metadata 
   graphic-hash  ; all graphics by id
   parent-hash   ; graphic-id to parent graphic
   graphics)     ; top-level graphics
   #:transparent)

(struct Metadata 
  (notes         ; string or #f
   property-hash ; maybe empty 
   link)         ; Link/GraphicLink or #f
  #:transparent)

(struct Graphic (id metadata) #:transparent)

(struct Group Graphic (children) #:transparent)
(struct Subgraph Group (back-shape) #:transparent)

(struct Table Group 
  (grid) ; vector of rows, each row is a vector, each element is an id
  #:transparent)

(struct StyledGraphic Graphic (style) #:transparent)
(struct Shape StyledGraphic (bounds text shape rotation fill-color) #:transparent)
(struct Label     Shape (line-id position) #:transparent)
(struct Connector Shape (head-id tail-id) #:transparent)

(struct Line StyledGraphic 
  (head-id    ; or #f
   tail-id    ; or #f
   head-arrow
   tail-arrow
   type       ; 'straight, 'curved, 'orthogonal or 'bezier
   points) 
  #:transparent)

(struct Text (text) #:transparent)
(struct ColoredText Text (color) #:transparent)

(struct Point (x y) #:transparent)
(struct BezierEndPoint Point (cpx cpy) #:transparent)
(struct BezierMidPoint Point (cpx1 cpy1 cpx2 cpy2) #:transparent)
(struct Rectangle Point (w h) #:transparent)

(struct Link (canvas-index) #:transparent)
(struct GraphicLink Link (graphic-index) #:transparent)

(struct Style 
  (line-pattern    ; number, 0 = solid
   line-color      ; (r g b)
   line-thickness
   corner-radius)
  #:transparent)

;; Find the bounds of the graphics in a canvas as a (values x y w h)
;; If the canvas is empty then the values are all #f
(define (canvas->bounds canvas)
  (let ([x #f][y #f][x2 #f][y2 #f])
    (for-each (λ(g)
                (match g
                  [(Shape _ _ _ (Rectangle gx gy gw gh) _ _ _ _)
                   (set! x  (if x  (min x gx) gx))
                   (set! y  (if y  (min y gy) gy))
                   (set! x2 (if x2 (max x2 (+ gx gw)) (+ gx gw)))
                   (set! y2 (if y2 (max y2 (+ gy gh)) (+ gy gh)))]
                  [(Line _ _ _ _ _ _ _ _ points)
                   (for-each (λ(p)
                               (let ([px (Point-x p)]
                                     [py (Point-y p)])
                                 (set! x  (if x  (min x px) px))
                                 (set! y  (if y  (min y py) py))
                                 (set! x2 (if x2 (max x2 px) px))
                                 (set! y2 (if y2 (max y2 py) py)))) 
                             points)]
                  [_ (void)]))
                               
              (hash-values (Canvas-graphic-hash canvas)))
        
    (values (if x (exact-round x) #f)
            (if y (exact-round y) #f)
            (if x (exact-round (- x2 x)) #f) 
            (if y (exact-round (- y2 y)) #f))))