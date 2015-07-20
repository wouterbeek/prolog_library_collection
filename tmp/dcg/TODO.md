# TODO

There are still several issues with DCGs that I have not solved yet.

## Preferred generation

In RFC 7230 HTTP header fields are defined as [1],
with optional whitespace defined as [2].

```bnf
[1] header-field = field-name ":" OWS field-value OWS
[2] OWS = *( SP / HTAB )
```

In addition to these grammar definitions there is the preference to generate
exactly one space (`SP` production) before a field value.
How to implement this concisely?


Requirements:
  - Parse zero or more `SP`s and/or `HTAB`s.
  - Generate exactly one `SP`.

Current 'solution':

```prolog
% Preferred: 'SP'. Allowed: 'OWS'.
(   'SP'
;   'OWS'
),
```

## Disjunction

How to use disjuction between DCGs in meta-arguments?

It would be a big improvement if we could write:

```prolog
comment -->
  bracketed(comment_inner, Comment)).

comment_inner -->
  ctext,
  comment_inner.
comment_inner -->
  'quoted-pair',
  comment_inner.
comment_inner -->
  comment.
```

as:

```prolog
comment -->
  bracketed((ctext ; 'quoted-pair' ; comment)).
```
