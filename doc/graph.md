<!--==================================================================-->
<!--    serrano/prgm/project/bigloo/5.0a/doc/string.md                -->
<!--    ----------------------------------------------------------    -->
<!--    Author      :  manuel serrano                                 -->
<!--    Creation    :  Mon Apr 13 10:38:02 2026                       -->
<!--    Last change :                                                 -->
<!--    Copyright   :  2026 manuel serrano                            -->
<!--    -----------------------------------------------------------   -->
<!--    Strings                                                       -->
<!--==================================================================-->

,(implementation-path "../runtime/Unsafe/ssr.scm")
,(example-path "../test/src/ssr.bgl")


Graph Algorithms
================

Single Source reachability
--------------------------

Single source reachability (SSR) graphs enable constant time ownership
of a node. They can be used to build graphs, add nodes and edges,
remove and replace edges, and then test if after such a modification a
certain node is still reachable from the root. A naive implementation
would traverse the whole graph from the root, paying attention to
cycle to check the ownership. SSR does this efficiently without any
traversal.

The algorithm assumes that nodes are represented by integer labels.

### ssr-make-graph ###
Builds an `ssr` graph.

### ssr-add-edge! ###
Adds an edge to an `ssr` graph. If `from` and `to`, two integers, are not
already members of the graph, they are added as well as a connecting edge.
The optional argument `onconnect` is a function of one argument. 

### ssr-remove-edge! ###
Removes the edge from an `ssr` graph connecting `from` and `to` (two integers).
The optional argument `ondisconnect` is a function of one argument. 

### ssr-redirect! ###
Redirects edges from an `ssr` graph. Edges pointing to `node` are redirected
to `other`. The optional arguments `onconnect` and `ondisconnect` are
functions of one argument.

### ssr-connected? ###
Returns `#t` is `node` is reachable from the graph source.

