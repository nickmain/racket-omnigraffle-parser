#lang racket
;; ------------------------------------------------------------------
;; Copyright (C) 2013, David Nick Main.
;;
;; See LICENSE file for details of license terms.
;; ------------------------------------------------------------------

(require "diagram-model.rkt")
(require "plist.rkt")
(require "rtf.rkt")

(provide filename->graffle)

;; Create a Document model from an Omnigraffle file
(define (filename->graffle filename)
  (let* ([plist       (load-plist filename)]         
         ; if no Sheets list then entire plist is a single sheet
         [sheets      (hash-ref plist "Sheets" (list plist))]
         [canvases    (map sheet->canvas sheets)]         
         [canvas-hash (make-hash)])
    (for-each (λ(c)(hash-set! canvas-hash (Canvas-id c) c)) canvases)
    (Document filename canvas-hash canvases)))

(define (sheet->canvas sheet)
  (let* ([id           (hash-ref sheet "UniqueID" 0)]
         [title        (hash-ref sheet "SheetTitle" "?")]
         [graphic-hash (make-hash)]
         [parent-hash  (make-hash)] 
         [g-dicts      (hash-ref sheet "GraphicsList" null)]
         
         [graphics (map dict->graphic g-dicts)]
         [metadata (Metadata (rtf->plain (ref+ sheet '("BackgroundGraphic" "Notes") #f))
                             (ref+ sheet '("BackgroundGraphic" "UserInfo") (thunk (make-hash)))
                             #f)])    
    
    ; build the graphic and parent hashes
    (for-each (curry gather-graphics graphic-hash parent-hash) graphics)
    
    (Canvas id title metadata graphic-hash parent-hash graphics)))

;; recursive gathering of graphics 
(define (gather-graphics graphic-hash parent-hash graphic)  
  (let ([id (Graphic-id graphic)]
        [kids (match graphic
                [(Subgraph _ _ children back-shape) (cons back-shape children)]
                [(Group    _ _ children)            children]
                [_ null])])
    (hash-set! graphic-hash id graphic)
    (for-each (curry gather-graphics graphic-hash parent-hash) kids)
    (for-each (λ(kid)(hash-set! parent-hash (Graphic-id kid) graphic)) kids)))

(define (dict->graphic g-dict)
  (let* ([id    (hash-ref g-dict "ID" 0)]
         [class (hash-ref g-dict "Class" #f)]                  
         [metadata (Metadata (rtf->plain (hash-ref g-dict "Notes" #f))
                             (hash-ref g-dict "UserInfo" (thunk (make-hash)))
                             (case (ref+ g-dict '("Link" "documentJump" "Type") 0)
                               [(1) (GraphicLink (ref+ g-dict '("Link" "documentJump" "Worksheet") 0)
                                                 (ref+ g-dict '("Link" "documentJump" "Graphic"  ) 0))]
                               [(6) (Link (ref+ g-dict '("Link" "documentJump" "Worksheet") 0))]
                               [else #f]))])         
    
    
    (case class
      [("ShapedGraphic" 
        "LineGraphic") (dict->styled-graphic id metadata class g-dict)]
      [("Group")       (dict->group id metadata g-dict)]
      [("TableGroup")  (dict->table id metadata g-dict)]
      [else (raise (~a "unhandled graphic class: " class))])))

(define (dict->styled-graphic id metadata class g-dict)
  (let ([style (Style  (ref+ g-dict '("Style" "stroke" "Pattern") 0)
                       (read-rgb-color (ref+ g-dict '("Style" "stroke") #f) '(0 0 0))
                       (if (equal? "NO" (ref+ g-dict '("Style" "stroke" "Draws") "YES"))
                           0 
                           (ref+ g-dict '("Style" "stroke" "Width") 1))
                       (ref+ g-dict '("Style" "stroke" "CornerRadius") 0))])    
    (case class
      [("ShapedGraphic") (dict->shape id style metadata g-dict)]
      [("LineGraphic")   (dict->line  id style metadata g-dict)]
      [else (raise (~a "unhandled styled graphic class: " class))])))


;; read an RGB color from a plist dict
(define (read-rgb-color dict default)
  (if dict
      (list (round (* 255 (string->number (ref+ dict '("Color" "r") "0"))))
            (round (* 255 (string->number (ref+ dict '("Color" "g") "0"))))
            (round (* 255 (string->number (ref+ dict '("Color" "b") "0")))))
      default))

(define (dict->shape id style metadata g-dict)
  (let* ([text     (rtf->plain (ref+ g-dict '("Text" "Text") #f))]
         [text-clr (read-rgb-color (ref+ g-dict '("FontInfo" "Color") #f) #f)]
         [shape    (hash-ref g-dict "Shape" #f)]
         [bounds   (get-bounds g-dict)]
         [rotation (ref+ g-dict '("Rotation") 0)]
         [line-id  (ref+ g-dict '("Line" "ID") #f)]
         [line-pos (ref+ g-dict '("Line" "Position") #f)]
         [head-id  (ref+ g-dict '("Head" "ID") #f)]
         [tail-id  (ref+ g-dict '("Tail" "ID") #f)]
         [fill     (if (equal? "NO" (ref+ g-dict '("Style" "fill" "Draws") "YES"))
                       #f          
                       (read-rgb-color (ref+ g-dict '("Style" "fill") #f) #f))])
    
    (when (equal? text "") (set! text #f))
    (when text
      (set! text
            (if text-clr
                (ColoredText text text-clr)
                (Text text))))
    
    (if line-id
        (Label id metadata style bounds text shape rotation fill line-id line-pos)
        (if (or head-id tail-id)
            (Connector id metadata style bounds text shape rotation fill head-id tail-id)
            (Shape id metadata style bounds text shape rotation fill)))))
    
(define (get-bounds g-dict) (parse-bounds (hash-ref g-dict "Bounds" "0,0,0,0")))

(define (dict->line id style metadata g-dict)
  (let* ([head-id    (ref+ g-dict '("Head" "ID") #f)]
         [tail-id    (ref+ g-dict '("Tail" "ID") #f)]
         [head-arrow (ref+ g-dict '("Style" "stroke" "HeadArrow") #f)]
         [tail-arrow (ref+ g-dict '("Style" "stroke" "TailArrow") #f)]
         [type (case (ref+ g-dict '("Style" "stroke" "LineType") #f)
                 [(1) (if (ref+ g-dict '("Style" "stroke" "Bezier") #f)
                          'bezier
                          'curved)]
                 [(2) 'orthogonal]
                 [else 'straight])]
         
         [ctrl-pnts (map parse-point (ref+ g-dict '("ControlPoints") null))]
         [pnts      (map parse-point (ref+ g-dict '("Points") null))]
         [points (if (null? ctrl-pnts)                     
                     (map (λ(p)(Point (first p)(second p))) pnts) ; non-bezier
                     ; bezier
                     (let ([mid-pnts       (drop-right (cdr pnts) 1)]
                           [mid-ctrl-pnts  (drop-right (cdr ctrl-pnts) 1)]
                           [first-pnt      (car pnts)]
                           [last-pnt       (last pnts)]
                           [first-ctrl-pnt (car ctrl-pnts)]
                           [last-ctrl-pnt  (last ctrl-pnts)])
                       (append (list (make-bezier-end-point first-pnt first-ctrl-pnt))
                               (make-bezier-mid-points mid-pnts mid-ctrl-pnts)
                               (list (make-bezier-end-point last-pnt last-ctrl-pnt)))))])
  
    (when (equal? "0" head-arrow) (set! head-arrow #f))
    (when (equal? "0" tail-arrow) (set! tail-arrow #f))
    (Line id metadata style head-id tail-id head-arrow tail-arrow type points)))    

(define (make-bezier-end-point pnt ctrl-pnt)
  (BezierEndPoint (first pnt)      (second pnt)
                  (first ctrl-pnt) (second ctrl-pnt)))
                 
(define (make-bezier-mid-points mid-pnts mid-ctrl-pnts)
  (if (null? mid-pnts)
      null
      (cons
       (BezierMidPoint (first (first  mid-pnts))      (second (first  mid-pnts))
                       (first (first  mid-ctrl-pnts)) (second (first  mid-ctrl-pnts))
                       (first (second mid-ctrl-pnts)) (second (second mid-ctrl-pnts)))
       (make-bezier-mid-points (cdr mid-pnts) (cddr mid-ctrl-pnts)))))


(define (dict->group id metadata g-dict)
    (let ([graphics (map dict->graphic (hash-ref g-dict "Graphics" null))]
          [subgraph (hash-ref g-dict "isSubgraph" #f)])
      
      (if subgraph
          (Subgraph id metadata (drop-right graphics 1) (last graphics))
          (Group id metadata graphics))))

(define (dict->table id metadata g-dict)
    (let* ([graphics (map dict->graphic (hash-ref g-dict "Graphics" null))]
           [gridh (hash-ref g-dict "GridH" #f)]
           [gridv (hash-ref g-dict "GridV" #f)]
           [grid (cond
                   [(not gridh) ; no cols - just a single row
                    (vector (list->vector gridv))]
                   [(not gridv) ; no rows - just a single col
                    (list->vector (map vector gridh))]
                   [else
                    (list->vector (map list->vector gridv))])])
      
       (Table id metadata graphics grid)))

(define (cells->rows cells)
  (let ([row-hashes (make-hash)]) ;;keyed by y
    ;; put cells in hashes keyed by y then x
    (for-each
     (λ(cell)(let* ([bounds (cdr cell)]
                    [x (vector-ref bounds 0)]
                    [y (vector-ref bounds 1)]
                    [row-hash (hash-ref! row-hashes y make-hash)])
               (hash-set! row-hash x (car cell))))
     cells)
    (let* ([ys (sort (hash-keys row-hashes) <)]
           [row-list (map (λ(y) (let* ([row-hash (hash-ref row-hashes y)]
                                       [xs (sort (hash-keys row-hash) <)]
                                       [row-ids (map (λ(x)(hash-ref row-hash x)) xs)])
                                  (list->vector row-ids)))
                          ys)])
      (list->vector row-list))))


;; "{{x,y}, {w,h}}" -> Rectangle
(define (parse-bounds bounds)
  (let ([nums (map string->number (regexp-split "," (regexp-replace* "[{| |}]" bounds "")))])
    (Rectangle (first nums) (second nums) (third nums) (fourth nums))))    

;; "{x,y}" -> (x y)
(define (parse-point point-string)
  (map string->number (regexp-split "," (regexp-replace* "[{| |}]" point-string ""))))

;; follow multiple keys thru nested hashs
(define (ref+ hash keys default)
  (foldl (λ(key hash)(if (hash? hash)
                         (hash-ref hash key default)
                         default))
         hash
         keys))

(module+ main 
  (printf "~a\n" (filename->graffle "../diagrams/test2.graffle")))