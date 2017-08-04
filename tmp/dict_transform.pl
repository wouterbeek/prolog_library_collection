:- module(
  dict_transform,
  [
  ]
).

/** <module> Dictionary transformations

from:

```swi
_{..., facets: [_{key: <KEY>, value: <VAL>}, ...], ...}
```

to:

```swi
_{..., <KEY>: <VAL>, ...}
```

@author Wouter Beek
@tbd
@version 2017/01
*/
