#lang racket/base

(require racket/class
         racket/gui/base
         sgl/gl)

;; Exercise reentrancy of `call-as-current`.
(define bm (make-gl-bitmap 32 32 (new gl-config%)))
(define ctx (send (make-object bitmap-dc% bm) get-gl-context))
(send ctx call-as-current
      (λ ()
        (glClearColor 0.0 0.0 0.0 0.0)
        (glClear GL_COLOR_BUFFER_BIT)
        (glBegin GL_TRIANGLES)
        (send ctx call-as-current
              (λ ()
                (glVertex2f -1.0 -1.0)
                (glVertex2f +1.0 -1.0)
                (glVertex2f -1.0 +1.0)))
        (glEnd)
        (glFinish)))
