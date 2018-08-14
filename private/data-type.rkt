#lang racket/base

(require racket/contract)

(provide
 (contract-out
  (struct zipper ([data list?] [context list?]))))

(struct zipper
  (data context)
  #:prefab)

