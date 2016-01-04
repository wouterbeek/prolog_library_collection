plTree
======

Support for tree datastructures in Prolog.

Author: [Wouter Beek](http://www.wouterbeek.com)



Installation
------------

```bash
$ git clone https://github.com/wouterbeek/plTree.git
$ cd plTree
$ git submodule update --init
$ swipl run.pl
```



Example of use
--------------

```prolog
?- use_module(tree).
?- edges_to_tree([a-b,a-c,a-d,b-e], Tree).
Tree = a-[b-[e-[]], c-[], d-[]].
```

```prolog
?- use_module(tree_print).
?- print_tree(a-[b-[],c-[d-[],e-[]],f-[]], []).
|- a
|-- b
|-- c
|--- d
|--- e
|-- f
true.
```
