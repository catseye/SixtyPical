TODO for SixtyPical
===================

Language
--------

### Save values to other-than-the-stack

Allow

    save a to temp_a {
        ...
    }

Which uses some other storage location instead of the stack.  A local non-static
would be a good candidate for such.  At any rate, the location must not
be writeable by anything that is called from within the block.  So, probably
just restrict this to local non-statics.

### Copy byte to/from table

Do we want a `copy bytevar, table + x` instruction?  We don't currently have one.
You have to `ld a`, `st a`.  I think maybe we should have one.

### Character literals

For goodness sake, let the programmer say `'A'` instead of `65`.

### Character set mapping

Not all computers think `'A'` should be `65`.  Allow the character set to be
mapped.  Probably copy what Ophis does.

### Pointers into non-byte tables

Right now you cannot get a pointer into a non-byte (for instance, word or vector) table.

Word and vector tables are stored as two byte tables in memory.  This is useful for
indexed access, but makes pointer access more difficult.

Laying them out for pointer access would make indexed access more difficult.

### Saving non-byte values

Right now you cannot save a word value.

There doesn't seem to be a hugely pressing reason why not.

Analysis
--------

### Forbid recursion

What happens if a routine calls itself, directly or indirectly?  Many
constraints might be violated in this case.  We should probably disallow
recursion by default.  (Which means assembling the callgraph in all cases.)

### Analyze memory usage

If you define two variables that occupy the same address, an analysis error ought
to be raised.  (But there should also be a way to annotate this as intentional.
Intentionally making two tables overlap could be valuable.  However, the analysis
will probably completely miss this fact.)

Optimization
------------

### Space optimization of local non-statics

If there are two routines A and B, and A never calls B (even indirectly), and
B never calls A (even indirectly), then their non-static locals can
be allocated at the same space.

This is not just an impressive trick -- in the presence of local pointers, which
use up a word in zero-page, which we consider a precious resource, it allow those
zero-page locations to be re-used.

Implementation
--------------

### Filename and line number in analysis error messages

For analysis errors, there is a line number, but it's the line of the routine
after the routine in which the analysis error occurred.  Fix this.

Blue-skying
-----------

### Pointers associated globally with a table(?)

We have `point into` blocks, but we would also like to sometimes pass a pointer
around to different routines, and have them all "know" what table it operates on.

We could associate every pointer variable with a specific table variable, in its
declaration.  This makes some things simple, and would allow us to know what table a
pointer is supposed to point into, even if that pointer was passed into our routine.

One drawback is that it would limit each pointer to be used only on one table.  Since a
pointer basically represents a zero-page location, and since those are a relatively scarce
resource, we would prefer if a single pointer could be used to point into different tables
at different times.

These can co-exist with general, non-specific-table-linked `pointer` variables.

If we have local pointers and space optimization for local non-statics, though,
these don't add as much.
