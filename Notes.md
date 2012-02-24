Peculiarities of Boolean program tool input formats
------------------------------------------------------

### Getafix does not like
- multiple labels per statement
- multiple targets per goto
- schoose
- the `dead` statement

### Moped does not like
- multiple labels per statement  
- "dead" statements
- "constrain" clauses

Wish list
------------

### Things I might want from Boogie
- `/stratifiedInline:1` only inlines certain procedures -- i.e., not the ones
  that I want to use summarization for.  This might already be the case, but
  at least when I use `/extractLoops:N`, the loops on which I've annotated
  invariants are still unrolled.
