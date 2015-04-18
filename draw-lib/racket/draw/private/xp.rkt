#lang racket/base
(require ffi/unsafe
         ffi/winapi)

;; Unfortunately, we sometimes need to do something different
;;  under Windows XP

(provide xp?)

(define xp? 
  (and (eq? 'windows (system-type))
       (let* ([GetVersion (get-ffi-obj 'GetVersion
				       (ffi-lib "kernel32.dll")
				       (_fun #:abi winapi -> _int32))])
	 (= 5 (bitwise-and #xFF (GetVersion))))))
