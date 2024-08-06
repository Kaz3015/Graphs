;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname HW12) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Exercise 1

(define-struct node [label])
;; A Node is a (make-node String)
;; Interpretation: A (make-node label) represents the label of a node in a graph
;; Examples:
(define ND1 (make-node "A"))
(define ND2 (make-node "B"))
(define ND3 (make-node "C"))
(define ND4 (make-node "D"))
(define ND5 (make-node "E"))

(define-struct edge [start end direction])
;; An Edge is a (make-edge String Node Node Boolean)
;; Interpretation: A (make-edge label start end both-ways?) contains the edge's label, the start and
;; end node of the edge, and whether or not it is a two-way connection.
;; Examples:
(define EDGE-1 (make-edge "A->B" ND1 ND2))
(define EDGE-2 (make-edge "C->D" ND3 ND4))
(define EDGE-3 (make-edge "E->B" ND5 ND2))

(define-struct elgraph [nodes edges])
;; An ELGraph is a (make-elgraph [List-of Nodes] [List-of Edges])
;; Interpretation: An ELGraph represents a graph with labeled nodes and edges
;; Examples:
(define ELG-1 (make-elgraph (list ND1 ND2 ND3 ND4 ND5) (list EDGE-1 EDGE-2 EDGE-3)))
(define ELG-2 (make-elgraph (list ND1 ND2) (list EDGE-1)))
(define ELG-3 (make-elgraph (list ND1 ND2 ND3 ND4) (list EDGE-1 EDGE-2)))

;; Exercise 2

(define HU (make-node "Huntington and Opera"))
(define OP (make-node "Opera and St Stephens"))
(define ST (make-node "St Stephens and gainsborough"))
(define GA (make-node "Gainsborough and Hemmingway"))
(define HE (make-node "Hemmingway and Forsyth"))
(define FO (make-node "Forsyth and huntington"))
(define HH (make-node "Huntingtom and hemmingway"))

(define HU->OP (make-edge HU OP "north"))
(define OP->ST (make-edge OP ST "east"))
(define ST->GA (make-edge ST GA "north"))
(define GA->HE (make-edge GA HE "west"))
(define HE->FO (make-edge HE FO "east"))
(define FO->HU (make-edge FO HU "east"))
(define HU->HE (make-edge HU HE "north"))

(define OP->HU (make-edge HU OP "south"))
(define ST->OP (make-edge OP ST "west"))
(define GA->ST (make-edge ST GA "south"))
(define HE->GA (make-edge GA HE "east"))
(define FO->HE (make-edge HE FO "west"))
(define HU->DO (make-edge FO HU "west"))
(define HE->HU (make-edge HU HE "south"))

(define given-street-graph (make-elgraph (list HU OP ST GA HE FO)
                                         (list HU->OP OP->ST ST->GA GA->HE HE->FO FO->HU HU->HE)))

(define A (make-node "Hooper cooper place"))
(define B (make-node "Cooper place and Opper ave"))
(define C (make-node "Opper ave, people ave, and Hopper st"))
(define D (make-node "People ave and morg ave"))
(define E (make-node "morg ave"))

(define A->B (make-edge A B "north"))
(define B->C (make-edge B C "east"))
(define C->A (make-edge C A "south west"))
(define C->D (make-edge C D "south"))
(define D->E (make-edge D E "south"))

(define my-street-graph (make-elgraph (list A B C D E) (list A->B B->C C->A C->D D->E)))

;; Exercise 3
;; driving-directions : String String ELGraph -> [List-of Edges]
;; Returns the path from the start node to the end node
(check-expect (driving-directions (node-label HE) (node-label ST) given-street-graph)
              (list "east to Forsyth and huntington"
                    "east to Huntington and Opera"
                    "north to Opera and St Stephens"
                    "east to St Stephens and gainsborough"))
(check-expect (driving-directions (node-label A) (node-label D) my-street-graph)
              (list
               "north to Cooper place and Opper ave"
               "east to Opper ave, people ave, and Hopper st"
               "south west to Hooper cooper place"))
(check-expect (driving-directions (node-label GA) (node-label ST) given-street-graph)
              (list
               "west to Hemmingway and Forsyth"
               "east to Forsyth and huntington"
               "east to Huntington and Opera"
               "north to Opera and St Stephens"
               "east to St Stephens and gainsborough"))
(define (driving-directions start end graph)
  (lucky start end (elgraph-edges graph) graph))

;; find-node : String [List-of Edges] -> Edge
;; Finds an edge that starts with the given node
(check-expect (find-node "A" empty) empty)
(check-expect (find-node "St Stephens and gainsborough" (list HU->OP OP->ST)) empty)
(check-expect (find-node (node-label HU) (list HU->OP OP->ST ST->GA))
              (make-edge (make-node "Huntington and Opera") (make-node "Opera and St Stephens") "north"))
(define (find-node node loE)
  (cond
    [(empty? loE) empty]
    [(cons? loE) (if (string=? node (node-label (edge-start (first loE))))
                     (first loE) (find-node node (rest loE)))]))

;; lucky : String String [List-of Edges] ELGraph -> [List-of Strings]
;; Checks if the start and end node are the same
(check-expect (lucky "A" "A" (list EDGE-1 EDGE-2 EDGE-3) given-street-graph) (list "you're there"))
(check-expect (lucky "Hooper cooper place" "Cooper place and Opper ave"
                     (list A->B B->C C->D) my-street-graph)
              (list "north to Cooper place and Opper ave"))
(define (lucky node end loE graph)
  (if (string=? node end) (list "you're there")
      (checker (find-node node loE) loE end (list) (list node) graph)))

;; sorter : Edge [List-of Edges] Node Accumulator [List-of Nodes] ELGraph -> [List-of Strings]
;; Finds another node to go to
(define (sorter edge loE end acc visited-nodes graph)
  (cond
    [(empty? loE) acc]
    [(cons? loE) (if
                  (and (not-visited? edge visited-nodes)
                       (string=? (node-label (edge-end edge)) (node-label (edge-start (first loE)))))
                  (checker (first loE) (elgraph-edges graph) end acc
                           (append visited-nodes
                                   (list (node-label (edge-start edge)))) graph)
                  (sorter (thing edge (elgraph-edges graph) visited-nodes)
                          (rest loE) end acc visited-nodes graph))]))

;; thing : Edge [List-of Edges] [List-of Strings] -> Edge
;; Checks if the starting edge's node has been visited. If so, finds a new edge;
;; otherwise, returns the edge given
(define (thing edge loE visited-nodes)
  (if (not-visited? edge visited-nodes) edge (find-different-edge edge loE)))

;; find-different-edge : Edge [List-of Edges] -> Edge
;; Finds an edge with the same starting edge as the given
(check-error (find-different-edge HU->OP (list HU->OP OP->ST ST->GA)))
(check-error (find-different-edge ST->GA (elgraph-edges given-street-graph)))
(check-expect (find-different-edge C->A
                                   (elgraph-edges my-street-graph))
              (make-edge
               (make-node "Opper ave, people ave, and Hopper st")
               (make-node "People ave and morg ave") "south"))
(define (find-different-edge edge loE)
  (first (filter
          (lambda (y) (string=? (node-label (edge-start edge)) (node-label (edge-start y))))
          (filter
           (lambda (x) (not (string=? (node-label (edge-end edge))
                                      (node-label (edge-end x))))) loE))))

;; checker : Edge [List-of Edges] String Accumulator [List-of Nodes] ELGraph -> [List-of Strings]
;; Checks if it’s at the last node; if it is it returns accumulator,
;; if not keeps going down more nodes
(check-expect (checker HU->OP empty "Opera and St Stephens" (list) empty given-street-graph) (list "north to Opera and St Stephens"))
(define (checker edge loE end acc visited-nodes graph)
  (if
   (string=? (node-label (edge-start edge)) end)
   acc 
   (sorter edge loE end
           (append acc
                   (list (string-append (edge-direction edge) " to "
                                        (node-label (edge-end edge))))) visited-nodes graph)))

;; not-visited? : Edge [List-of Strings] -> Boolean
;; Checks if the nodes in the list were visited
(check-expect (not-visited? HU->OP empty) #true)
(check-expect (not-visited? HU->OP (list "Huntington and Opera" "Opera and St Stephens")) #false)
(check-expect (not-visited? FO->HU
                            (list "Opera and St Stephens" "St Stephens and Gainsborough")) #true)
(define (not-visited? edge visited-nodes)
  (andmap (lambda (x) (not (string=? x (node-label (edge-end edge))))) visited-nodes))