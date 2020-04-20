#lang racket

(provide node-glyph
         node-x
         node-y
         test-program)

(struct node (glyph value parent x y))

(define test-program (list (node "+" + '() 0 0)
                           (node "2" 2 '(0) 0 1)
                           (node "3" 3 '(0) 0 -1)))

(define (find-root-ip input)
  (index-where input (λ (node) (null? (node-parent node)))))

(define (find-children ip in)
  (indexes-where in (λ (node) (memq ip (node-parent node)))))

(define (interpret input)
  (define root-ip (find-root-ip input))
  (define (rec ip)
    (define current-instruction (node-value (list-ref input ip)))
    (cond
      [(procedure? current-instruction)
       (apply current-instruction (map rec (find-children ip input)))]
      [else current-instruction]))
  (rec root-ip))

(interpret test-program)