#!/bin/sh

PROG=sixtypical

if [ x`which ghc` = x -a x`which runhugs` = x ]; then
    echo "Neither ghc nor runhugs found on search path."
    exit 1
fi

mkdir -p bin

if [ x`which ghc` = x -o ! x$USE_HUGS = x ]; then
    # create script to run with Hugs
    cat >bin/$PROG <<'EOF'
#!/bin/sh
THIS=`realpath $0`
DIR=`dirname $THIS`/../src
runhugs $DIR/Main.hs $*
EOF
    chmod 755 bin/$PROG
else
    cd src && ghc --make Main.hs -o ../bin/$PROG
fi
