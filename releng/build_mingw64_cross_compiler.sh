#!/bin/bash
set -e -x

rm -rf solib-native
mv solib solib-native

# Set up cross-compiler environment
eval `rpm --eval %{mingw64_env}`

export JDKDIR=/opt/jdk-11-win32

make Makefile_MINGW_CROSS
make -f Makefile_MINGW_CROSS javawrapper CBFLIB_DONT_BUILD_HDF5=yes CBF_NO_REGEX=yes CBFLIB_DONT_USE_LOCAL_REGEX=yes CBFLIB_DONT_USE_PYCIFRW=yes CBFLIB_DONT_USE_PY2CIFRW=yes CBFLIB_DONT_USE_PY3CIFRW=yes

DEST=/io/dist/$VERSION/win32/$PLAT/$ARCH
mkdir -p $DEST
cp $JARFILE $DEST
cp solib/cbf*.dll $DEST

