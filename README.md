# Prim's Algorithm implementation in Haskell, with several performance optimization, using red-black tree data structure.

This is a semester project for Non-procedural Programming (NPRG005) course.

Some implementation details:

In order to implement Prim's algorithm there is a need in priority queue data structure.

In a naive implementation of such a structure, an unordered list can be used, then insertions will take O(1) (a simple prepend) and removing the smallest element will take O(N).

In order to optimize the algorithm a more efficient data structure was used, which is Red-black tree. (can be found here: https://github.com/AyazDyshin/Red-Black-Tree-Haskell). In such case insertions will run in O(log N), and removing the smallest element will also take O(log N).

Problem with implementation of decreaseKey:

If red-black tree is used then to find a given value in the tree the algorithm will need to search the entire tree, which will take O(N) time.

Solution:

To solve this issue a following trick was used:

If the queue contains some value V with key K, and there is a need to decrease the key to K', then algorithm can simply insert V into the queue again with key K'.  That will take time O(log N).  After that, V will exist twice in the queue, once with priority K and once with priority K'. Once K' becomes the smallest key in the queue, an extractMin operation will remove it.  Now (V, K) is still in the queue.  At some later time extractMin will remove (V, K).  At this point algoritm must ignore V since you have already processed it.

To do that, there needs to be some fast way to check whether a vertex V has already been processed, i.e. is in the spanning tree.

First approach is to simply search through a list of all vertices in the tree, but that will take O(N).

A better approach is to have a second red-black tree that stores the set of vertices that are currently in the tree.  Then algoritm can add to that set in O(log N), and also look up in O(log N).

Tests folder contains two files with sample graph data input to test the algorithm.

Original date of submission:
08.08.2020
