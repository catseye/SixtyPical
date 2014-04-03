#!/bin/sh

FILES="doc/Checking.markdown doc/Emitting.markdown doc/Instruction_Support.markdown"
./build.sh && falderal --substring-error ${FILES}
