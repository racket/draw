#lang racket/base
(require racket/class
         racket/draw
         racket/file)

;; Check memory-management in the bitmap/PNG/JPEG/etc. library by reading
;; and writing in many threads at the same time.

(define (check src save-type [read-type 'unknown/alpha])
  (printf "~s ~s\n" save-type read-type)
  (define ts
    (for/list ([i (in-range 1 #;40)])
      (thread
       (lambda()
         (for ([i (in-range 10)])
           (define bm (read-bitmap (collection-file-path src "icons")))
           (define t (make-temporary-file))
           (send bm save-file t save-type)
           (define bm2 (read-bitmap t read-type))
           (define w (send bm get-width))
           (define h (send bm get-width))
           (define s1 (make-bytes (* w h 4)))
           (define s2 (make-bytes (* w h 4)))
           (send bm get-argb-pixels 0 0 w h s1)
           (send bm2 get-argb-pixels 0 0 w h s2)
           (case save-type
             [(jpeg)
              ;; JPEG is lossy, so use a fuzzy compare:
              (define diff (for/sum ([b1 (in-bytes s1)]
                                     [b2 (in-bytes s2)])
                                    (- b2 b1)))
              (unless ((abs diff) . < . (* w h 1))
                (error 'bitmap-stress "mismatch for ~s ~s: ~s ~s ~e" 
                       src save-type 
                       w h diff))]
             [else
              (unless (equal? s1 s2)
                (error 'bitmap-stress "mismatch for ~s ~s" src save-type))])
           (delete-file t))))))

  (for ([t (in-list ts)]) (sync t)))

(check "PLT-206.png" 'png)
(check "plt.jpg" 'jpeg)
(check "htdp-icon.gif" 'png 'unknown)
(check "help16x16.xpm" 'png 'unknown)
(check "help16x16.xbm" 'png 'unknown)
(check "help.bmp" 'png 'unknown)

;; Also check that we don't run out of C stack space due to error escapes
(define (ones)
  (if (eq? 'racket (system-type 'vm))
      ;; For the 'racket VM, we can further check that the port is read on demand
      (make-input-port
       'ones
       (lambda (bstr)
         (bytes-set! bstr 0 1)
         1)
       (lambda (bstr . args)
         (bytes-set! bstr 0 1)
         1)
       void)
      ;; Assume other VMs read the poirt eagerly
      (open-input-bytes (bytes 1 1 1 1 1 1 1 1))))

(for ([i (in-range 25000)])
  (when (zero? (modulo i 1000))
    (printf "~s\n" i))
  (with-handlers ([exn:fail? void])
    (read-bitmap (ones) 'jpeg)))
