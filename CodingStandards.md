# Code Standards

In order to keep code short and interpretable I try to maintain certain patterns / conventions thoughout the codebase.  I do not believe that a computer program can be automatically formatted to look a specific way.  In the end, the way in which the program is written down into text is something that should optimize for a human reader (including the author).  Therefore, the presentation of a program is inherently something that cannot be automated.  However, it still makes sense to come up with certain patters / conventions that are often applicable.

## Indentation

2 spaces.  2, because 4 or 8 causes too much space to be wasted for no aparent reason.  (I am not sure why I do not use 1 or 3 spaces.)  Spaces, because that makes it easier to anticipate how code looks in different editors and configurations (where tabs will mess up meaningful alignments and ASCII artistry).

## Newlines

The US-ASCII newline character (Unix convention).

Every text file should end with a newline character.  This means that every line ends with the same character (uniformity).  It also means that line counting can be unambiguously applied.  Another benefit is that when a file is listed from the command line the prompt does not het cought up in the middle of the last listed line (but appears in the left hand side of the next line, where we expect it to be).

## Character margin

80, because this is the most common setting in other programs and it allows 2 buffers to be displayed next to each other on most displays.

## Abbreviations

We use the following abbreviations for often occurring variable names:

| **Variable name** | **Expansion**  |
|:-----------------:|:--------------:|
| A                 | Atom           |
| Abs               | Absolute       |
| Arg               | Argument       |
| Attr              | Attribute      |
| C                 | Code           |
| Char              | Character      |
| Col               | Column         |
| Comp              | Component      |
| D                 | Digit          |
| Def               | Default        |
| Descr             | Description    |
| E                 | Error          |
| Ext               | Extension      |
| Frac              | Fractional     |
| H                 | Head           |
| I                 | Integer        |
| L                 | List           |
| Lang              | Language       |
| Lbl               | Label          |
| LTag              | Language tag   |
| Max               | Maximum        |
| Min               | Minimum        |
| N                 | Number         |
| Num$X$s           | NumberOf$X$s   |
| Op                | Operator       |
| Opt               | Options        |
| Param             | Parameter      |
| Perc              | Percentage     |
| Pos               | Position       |
| Req               | Request        |
| S                 | String         |
| Sep               | Separator      |
| T                 | Tail           |
| Trans             | Transformation |
