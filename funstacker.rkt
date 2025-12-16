#lang br/quicklang

(define (read-syntax path port)
  (define src-lines (port->lines port))
  (define src-datums (format-datums '~a src-lines))
  (define module-datum `(module stacker-mod "funstacker.rkt"
                         (handle-args ,@src-datums)))
  (datum->syntax #f module-datum))
(provide read-syntax)

(define-macro (funstacker-module-begin HANDLE-ARGS-EXPR)
  #'(#%module-begin
     (display (first HANDLE-ARGS-EXPR))))
(provide (rename-out [funstacker-module-begin #%module-begin]))

(define (handle-args . args)
  (for/fold ([stack-acc empty]) ;; comeca acumulador stack-acc
            ([arg (in-list args)] ;; para cada arg na lista args
             #:unless (void? arg)) ;;itera a nao ser se for void, voids sao data types de datums q sao blank
    (cond
      [(number? arg) (cons arg stack-acc)]
      [(or (equal? * arg) (equal? + arg))
       (define op-result
         (arg (first stack-acc) (second stack-acc)))
       (cons op-result (drop stack-acc 2))])))
(provide handle-args)
(provide + *)
