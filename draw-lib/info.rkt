#lang info

(define collection 'multi)

(define deps
  '(("base" #:version "8.0.0.8")
    ("draw-i386-macosx-3" #:platform "i386-macosx")
    ("draw-x86_64-macosx-3" #:platform "x86_64-macosx")
    ("draw-ppc-macosx-3" #:platform "ppc-macosx")
    ("draw-aarch64-macosx-3" #:platform "aarch64-macosx")
    ("draw-win32-i386-3" #:platform "win32\\i386")
    ("draw-win32-x86_64-3" #:platform "win32\\x86_64")
    ("draw-x86_64-linux-natipkg-3" #:platform "x86_64-linux-natipkg")
    ("draw-x11-x86_64-linux-natipkg" #:platform "x86_64-linux-natipkg")
    ("draw-ttf-x86_64-linux-natipkg" #:platform "x86_64-linux-natipkg")))

(define pkg-desc "implementation (no documentation) part of \"draw\"")

(define pkg-authors '(mflatt))

(define version "1.18")
