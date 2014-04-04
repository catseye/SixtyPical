#!/bin/sh

FILES="doc/Checking.markdown
       doc/Emitting.markdown
       doc/Instruction_Support.markdown
       doc/Analyzing.markdown"
./build.sh && falderal --substring-error ${FILES}
