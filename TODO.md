TODO for SixtyPical
===================

### `low` and `high` address operators

To turn `word` type into `byte`.

Trying to remember if we have a compelling case for this or now.  The best I can think
of is for implementing 16-bit `cmp` in an efficient way.  Maybe we should see if we
can get by with 16-bit `cmp` instead though.

The problem is that once a byte is extracted, putting it back into a word is awkward.
The address operators have to modify a destination in a special way.  That is, when
you say `st a, >word`, you are updating `word` to be `word & $ff | a << 8`, somelike.
Is that consistent with `st`?  Well, probably it is, but we have to explain it.
It might make more sense, then, for it to be "part of the operation" instead of "part of
the reference"; something like `st.hi x, word`; `st.lo y, word`.  Dunno.

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

In the analysis, when we obtain a pointer, we need to record, in contect, what buffer
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

More generally, define a block as having zero or one `goto`s at the end.  (and `goto`s cannot
appear elsewhere.)

If a block ends in a `call` can that be converted to end in a `goto`?  Why not?  I think it can.
The constraints should iron out the same both ways.

And - once we have this - why do we need `goto` to be in tail position, strictly?
As long as the routine has consistent type context every place it exits, that should be fine.

### "Include" directives

Search a searchlist of include paths.  And use them to make libraries of routines.

One such library routine might be an `interrupt routine` type for various architectures.
Since "the supervisor" has stored values on the stack, we should be able to trash them
with impunity, in such a routine.
