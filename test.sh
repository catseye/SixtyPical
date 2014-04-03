#!/bin/sh

FILES="doc/Checking.markdown doc/Emitting.markdown"
./build.sh && falderal --substring-error ${FILES}
