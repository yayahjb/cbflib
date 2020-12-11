#!/bin/bash
set -e -x

# docker run --rm -v $(pwd):/io quay.io/pypa/manylinux2010_x86_64 /bin/bash /releng/build_manylinux_binding.sh
# bash build_manylinux_binding.sh

cd /io

# need jdk, pcre and swig (these work for both 2010 and 2014)
yum install -y java-1.8.0-openjdk-devel pcre-devel

if [ $PLAT = "manylinux2014_x86_64" ]; then
  yum install -y swig
else # need to build and install new swig
  SWIGPREFIX=swig-2.0.10-5
  SWIGSRC=${SWIGPREFIX}.el7.src.rpm
  curl -fsSLO https://vault.centos.org/7.9.2009/os/Source/SPackages/$SWIGSRC
  yum install -y guile-devel rpm-build python-devel dos2unix perl-devel perl-Test-Simple boost-devel
  rpmbuild --rebuild $SWIGSRC --without testsuite
  rpm -i /root/rpmbuild/RPMS/x86_64/${SWIGPREFIX}*.rpm
fi

JBIN=$(readlink -f `which java`)
export JDKDIR=$(dirname $(dirname $(dirname $JBIN)))

make Makefiles
make distclean
make javawrapper javatests CBFLIB_DONT_BUILD_HDF5=yes CBFLIB_DONT_USE_LOCAL_REGEX=yes CBFLIB_DONT_USE_PYCIFRW=yes

JARFILE="jcbf/cbflib-*.jar"
VERSION=`basename $JARFILE | sed -e 's/cbflib-\(.*\)\.jar/\1/g'`

DEST=/io/dist/$VERSION/linux/$PLAT
mkdir -p $DEST
cp $JARFILE $DEST
cp solib/libcbf*.so $DEST
cp /usr/lib64/libpcre.so $DEST
