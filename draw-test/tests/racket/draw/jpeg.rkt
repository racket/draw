#lang racket/base
(require racket/class
         racket/draw
         racket/port
         racket/file)

;; Try JPEG reading and writing through ports that require synchronization

(define bstr (file->bytes (collection-file-path "wizard-image.jpg" "icons")))

(define-values (i o) (make-pipe 16))

(void (thread (lambda ()
                (write-bytes bstr o)
                (close-output-port o))))

(define bm (read-bitmap i))

(define-values (i2 o2) (make-pipe 16))
(define o3 (open-output-bytes))

(define copy-t
  (thread (lambda ()
            (copy-port i2 o3)
            (close-output-port o3))))

(void (send bm save-file o2 'jpeg))
(close-output-port o2)

(thread-wait copy-t)
