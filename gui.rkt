#lang racket/gui

(require pict)

(define (pict-color pict)
  ;; unreliable if pict doesn't use 'colorize'
  (if (procedure? (second (pict-draw pict)))
      #f
      (second (pict-draw pict))))

(define (cell size -x +x -y +y)
  (letrec ([spike-scale 1/6]
           [extension (/ (sqrt (* (expt (* size spike-scale) 2) 2)) 2)]
           [neutral-cell (inset (filled-rectangle size size) extension)]
           [spike (rotate (scale (filled-rectangle size size) spike-scale) (/ pi 4))])
    (define (rec pict spikes)
      (define (make-spikes spikes color)
        (match (length spikes)
          [4 (rec (lc-superimpose pict (colorize spike color)) (rest spikes))]
          [3 (rec (rc-superimpose pict (colorize spike color)) (rest spikes))]
          [2 (rec (cb-superimpose pict (colorize spike color)) (rest spikes))]
          [1 (rec (ct-superimpose pict (colorize spike color)) (rest spikes))]))
      (cond [(null? spikes) pict]
            [(positive? (first spikes)) (make-spikes spikes "black")]
            [(negative? (first spikes)) (make-spikes spikes "white")]
            [(zero? (first spikes)) (rec pict (rest spikes))]))
    (if (for/and ([i (list -x +x -y +y)]) (zero? i))
        neutral-cell
        (rec neutral-cell (list -x +x -y +y)))))

(define (add-cell-glyphs cell main-glyph)
  (cc-superimpose cell main-glyph))

;;CLASSES
(define fancy-canvas%
  (class canvas%
    (inherit get-width get-height)
    (super-new)
    (init-field
     [grid-size 10])
    (define/public (get-grid-size) grid-size)
    (define/public (set-grid-size! n) (set! grid-size n))
    (define/public (get-cell-size) (/ (get-height) (add1 grid-size)))
    (define/public (grid-x->canvas-x n) (+ (/ (get-width) 2)
                                           (* (- n 1/2) (get-cell-size))))
    (define/public (grid-y->canvas-y n) (+ (/ (get-height) 2)
                                           (- (* (+ n 1/2) (get-cell-size)))))))

;; INSTANCES
(define main-window
  (new frame% [label "APL2D"]))

(define apl2d-canvas
  (new fancy-canvas%
       [parent main-window]
       [paint-callback (λ (canvas dc)
                         (draw-pict
                          (add-cell-glyphs
                           (cell (send canvas get-cell-size) -1 1 0 0)
                           (text
                            "⍋"
                            (cons (make-object color% 255 255 255 1.0) "APL385 Unicode")
                            (ceiling (/ (send canvas get-cell-size) 2))
                            0))
                          dc
                          (send canvas grid-x->canvas-x 0)
                          (send canvas grid-y->canvas-y 0)))]))

(send* main-window
  (maximize #t)
  (show #t))