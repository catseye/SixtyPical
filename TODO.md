TODO for SixtyPical
===================

### 16-bit `cmp`

This is because we don't actually want `low` and `high` address operators
that turn `word` type into `byte`.

This is because this immediately makes things harder (that is, effectively
impossible) to analyze.

16-bit `cmp` also benefits from some special differences between `cmp`
and `sub` on 6502, so it would be nice to capture them.

### Save values to other-than-the-stack

Allow

    save a to temp_a {
        ...
    }

Which uses some other storage location instead of the stack.  A local static
would be a good candidate for such.

### Associate each pointer with the buffer it points into

Check that the buffer being read or written to through pointer, appears in appropriate
inputs or outputs set.

In the analysis, when we obtain a pointer, we need to record, in context, what buffer
that pointer came from.

When we write through that pointer, we need to set that buffer as written.

When we read through the pointer, we need to check that the buffer is readable.

### Table overlays

They are uninitialized, but the twist is, the address is a buffer that is
an input to and/or output of the routine.  So, they are defined (insofar
as the buffer is defined.)

They are therefore a "view" of a section of a buffer.

This is slightly dangerous since it does permit aliases: the buffer and the
table refer to the same memory.

Although, if they are `static`, you could say, in the routine in which they
are `static`, as soon as you've established one, you can no longer use the
buffer; and the ones you establish must be disjoint.

(That seems to be the most compelling case for restricting them to `static`.)

An alternative would be `static` pointers, which are currently not possible because
pointers must be zero-page, thus `@`, thus uninitialized.

### Question "consistent initialization"

Question the value of the "consistent initialization" principle for `if` statement analysis.

Part of this is the trashes at the end; I think what it should be is that the trashes
after the `if` is the union of the trashes in each of the branches; this would obviate the
need to `trash` values explicitly, but if you tried to access them afterwards, it would still
error.

### Tail-call optimization

If a block ends in a `call` can that be converted to end in a `goto`?  Why not?  I think it can,
if the block is in tail position.  The constraints should iron out the same both ways.

And - once we have this - why do we need `goto` to be in tail position, strictly?
As long as the routine has consistent type context every place it exits, that should be fine.

### "Include" directives

Search a searchlist of include paths.  And use them to make libraries of routines.

One such library routine might be an `interrupt routine` type for various architectures.
Since "the supervisor" has stored values on the stack, we should be able to trash them
with impunity, in such a routine.

### Line numbers in analysis error messages

For analysis errors, there is a line number, but it's the line of the routine
after the routine in which the analysis error occurred.  Fix this.
