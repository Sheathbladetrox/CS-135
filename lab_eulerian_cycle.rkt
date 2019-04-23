#lang eopl
;; Andrew Chuah
;; I pledge my honor that I have abided by the Stevens Honor System.

;; An undirected graph is given as a relation that is symmetric but
;; every (a b) and (b a) pair should be treated as equivalent
;; vertices will be labelled starting at one and incrementing by one each time.
;; Below are some undirected graphs
(define cycle6 '((1 2) (2 3) (3 4) (4 5) (5 6) (6 1) (2 1) (3 2) (4 3) (5 4) (6 5) (1 6)))
(define david '((3 1) (1 3) (4 2) (2 4) (5 3) (3 5) (6 4) (4 6) (1 5) (5 1) (2 6) (6 2)))
(define study '((1 6) (2 3) (3 1) (4 1) (5 1) (6 1) (2 2) (3 2) (1 3) (1 4) (1 5) (6 5) (5 6) (2 4) (4 2)))
(define test '((2 3) (4 5) (5 2) (2 1) (3 2) (1 2) (3 3) (5 4) (2 5) (1 1)))



;; Define degree
;; degree takes an undirected graph and a given vertex
;; this will return the number of edges connected to the vertex
;; to do this check how many edges start at the same vertex as the given one
;; A reflexive edge is special in that it counts as 2 edges but is only written once

;; Examples:
;; (degree cycle6 4) -> 2
;; (degree david 2) -> 2
;; (degree study 1) -> 4
;; (degree test 1) -> 3

;; Type Signature: (degree relation int) -> int

(define (degree relation vertex)
  (if (equal? relation '()) 0
      (if (and (= (car(car relation)) vertex)(= (car(cdr(car relation))) vertex))(+ 2 (degree (cdr relation) vertex))
          (if (= (car(car relation)) vertex)(+ 1 (degree (cdr relation) vertex)) (degree (cdr relation) vertex)))))

;; Define maxVertex
;; maxVertex takes an undirected graph
;; It should return the max vertex (greatest number)
;; Do this by recursively going through relation and storing the current max each time
;; a helper may be useful

;; Examples:
;; (maxVertex cycle6) -> 6
;; (maxVertex '()) -> 1
;; (maxVertex '((1 1) (3 7) (7 3) (2 4) (4 2))) -> 7

;; Type Signature: (maxVertex relation) -> int

(define (maxVertex relation)
  (if (null? relation) 1
      (if (maxVertex-helper relation (car(car relation)))(car(car relation))(maxVertex (cdr relation)))))
(define (maxVertex-helper relation int)
  (if (null? relation) #t
      (if (>= int(car(car relation)))(and #t (maxVertex-helper (cdr relation) int)) #f)))

;; Define has-eulerian-cycle?
;; This is given an undirected graph and should return if the graph contains a eulerian cycle
;; A eulerian cycle exists iff every vertex has an even degree
;; a helper will be needed to keep track of what vertex you are checking

;; Examples:

;; Type Signature: (has-eulerian-cycle? relation) -> boolean
;; (has-eulerian-cycle? cycle6) -> #t
;; (has-eulerian-cycle? david) -> #t
;; (has-eulerian-cycle? study) -> #t
;; (has-eulerian-cycle? test) -> #f

(define (has-eulerian-cycle? relation)
   (eulerian-helper relation relation))
(define (eulerian-helper relation relation2)
  (if (null? relation2) #t
      (if (=(modulo(degree relation (car(car relation2)))2)0) (and #t (eulerian-helper relation (cdr relation2))) #f)))

