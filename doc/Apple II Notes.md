Notes for building SixtyPical programs for Apple II
===================================================

And running them on `linapple`.

We'll do `eg/rudiments/add-pass.60p`.  It does nothing.

    bin/sixtypical --origin=0x2000 --output-format=raw eg/rudiments/add-pass.60p > add-pass.bin
    cp ~/scratchpad/linapple/res/Master.dsk sixtypical.dsk
    a2in B sixtypical.dsk ADD-PASS add-pass.bin
    a2ls sixtypical.dsk
    linapple -d1 sixtypical.dsk -autoboot

Next... we should do one that does something.
