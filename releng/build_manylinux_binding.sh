#!/bin/bash
set -e -x

# docker run --rm -v $PWD:/io:Z ghcr.io/diamondlightsource/manylinux-dls-2010_x86_64:latest /bin/bash /io/releng/build_manylinux_binding.sh
# bash build_manylinux_binding.sh

cd /io

JBIN=$(readlink -f `which java`)
export JDKDIR=$(dirname $(dirname $(dirname $JBIN)))

make Makefiles
make distclean
make javawrapper javatests CBFLIB_DONT_BUILD_HDF5=yes CBF_NO_REGEX=yes CBFLIB_DONT_USE_LOCAL_REGEX=yes CBFLIB_DONT_USE_PYCIFRW=yes CBFLIB_DONT_USE_PY2CIFRW=yes CBFLIB_DONT_USE_PY3CIFRW=yes

JARFILE="jcbf/cbflib-*.jar"
VERSION=`basename $JARFILE | sed -e 's/cbflib-\(.*\)\.jar/\1/g'`

DEST=/io/dist/$VERSION/linux/$PLAT/$ARCH
mkdir -p $DEST
cp $JARFILE $DEST
cp solib/libcbf*.so $DEST
