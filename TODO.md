TODO for SixtyPical
===================

### Save values to other-than-the-stack

Allow

    save a to temp_a {
        ...
    }

Which uses some other storage location instead of the stack.  A local static
would be a good candidate for such.

### Analyze `call` within blocks?

What happens if you call another routine from inside a `with interrupts off` block?

What happens if you call another routine from inside a `save` block?

What happens if you call another routine from inside a `point into` block?

What happens if you call another routine from inside a `for` block?

Remember that any of these may have a `goto` ... and they may have a second
instance of the same block (e.g. `with interrupts off` nested within
`with interrupts off` shouldn't be allowed to turn them back on after the
inner block has finished -- even if there is no `call`.)

These holes need to be plugged.

### Pointers associated globally with a table

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

### Local non-statics

Somewhat related to the above, it should be possible to declare a local storage
location which is not static.

In this case, it would be considered uninitialized each time the routine was
entered.

So, you do not have a guarantee that it has a valid value.  But you are guaranteed
that no other routine can read or modify it.

It also enables a trick: if there are two routines A and B, and A never calls B
(even indirectly), and B never calls A (even indirectly), then their locals can
be allocated at the same space.

A local could also be given an explicit address.  In this case, two locals in
different routines could be given the same address, and as long as the condition
in the above paragraph holds, that's okay.  (If it doesn't, the analyzer should
detect it.)

This would permit local pointers, which would be one way of addressing the
"same pointer to different tables" problem.

### Copy byte to/from table

Do we want a `copy bytevar, table + x` instruction?  We don't currently have one.
You have to `ld a`, `st a`.  I think maybe we should have one.

### Tail-call optimization

If a block ends in a `call` can that be converted to end in a `goto`?  Why not?  I think it can,
if the block is in tail position.  The constraints should iron out the same both ways.

As long as the routine has consistent type context every place it exits, that should be fine.

### "Include" directives

Search a searchlist of include paths.  And use them to make libraries of routines.

One such library routine might be an `interrupt routine` type for various architectures.
Since "the supervisor" has stored values on the stack, we should be able to trash them
with impunity, in such a routine.

### Line numbers in analysis error messages

For analysis errors, there is a line number, but it's the line of the routine
after the routine in which the analysis error occurred.  Fix this.
